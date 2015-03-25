class ContactsImporter
  TmpDir = "#{Rails.root}/tmp"
  PhoneRegex = /phone|number|phone number/i

  attr_reader :account, :project, :project_variables
  attr_accessor :rows, :column_specs

  def initialize(account, project)
    @account = account
    @project = project
    @project_variables = project.project_variables.all
    @rows = []
    @column_specs = []
  end

  def save_csv(file_param)
    if file_param.blank?
      return "You didn't specify a CSV file"
    end

    # TODO: don't read all the file in memory at once (use IO.copy_stream?)
    contents = file_param.read

    if contents.blank?
      return "CSV file must not be empty"
    end

    File.open(csv_filename, "wb") do |f|
      f.write contents
    end

    # Validate that all rows have the same length
    begin
      rows = read_csv
    rescue => ex
      return "Invalid CSV: #{ex}"
    end

    if rows.empty?
      return "The CSV file is blank"
    end

    unless rows.all? {|row| row.size == rows[0].size}
      return "Not all rows have the same number of columns"
    end

    headers = rows.first
    if headers.grep(PhoneRegex).empty?
      return "Missing 'Phone number' column"
    end

    nil
  end

  def guess_column_specs
    headers = read_csv.first
    headers.map do |col|
      col = col.strip

      case col
      when ""
        {action: 'ignore', name: col, id: ''}
      when PhoneRegex
        {action: 'phone_number', name: 'Phone number', id: ''}
      else
        implicit_variable = find_implicit_variable(col)
        if implicit_variable
          {action: 'existing_variable', name: implicit_variable.key, id: implicit_variable.key}
        else
          project_variable = find_project_variable(col)
          if project_variable
            {action: 'existing_variable', name: project_variable.name, id: project_variable.id}
          else
            {action: 'new_variable', name: col, id: ''}
          end
        end
      end
    end
  end

  def import
    read_csv

    # Drop headers
    @rows.shift

    # Check where is the phone column
    phone_number_index = @column_specs.index { |spec| spec['action'] == 'phone_number' }

    # Gather all phone numbers
    phones = @rows.map do |row|
      phone = row[phone_number_index].strip

      # If it looks like a phone number (could be a sip address), removing extraneous chars
      phone = phone.gsub /\s|\(|\)|\+|\-/, "" if phone =~ /(\d|\s|\(|\)|\+|\-)+/

      # Update the row so later we don't have to do this again
      row[phone_number_index] = phone

      phone
    end

    # Find all contacts address with those numbers
    contact_addresses = ContactAddress.where(project_id: @project.id, address: phones).includes(:contact).all

    # And index them by their address, so we can look them up faster
    contact_addresses_by_address = contact_addresses.index_by &:address

    project_variables_by_name = project_variables.index_by &:name

    created_count = 0
    updated_count = 0
    unchanged_count = 0

    # Now process all rows
    @rows.each do |row|
      phone = row[phone_number_index]

      # Find contact address or create one (and its associated contact)
      contact_address = contact_addresses_by_address[phone]
      if contact_address
        contact = contact_address.contact
        created = false
      else
        contact = project.contacts.new
        contact_address = contact.addresses.new address: phone
        contact.save!

        contact_addresses_by_address[phone] = contact_address
        created = true
      end

      updated = false

      # Process all cells according to spec
      row.each_with_index do |cell, index|
        cell = cell.strip

        spec = @column_specs[index]
        case spec['action']
        when 'new_variable', 'existing_variable'
          name = spec['name']

          implicit_variable = implicit_variables.find { |var| var.key == name }
          if implicit_variable
            persisted_var = contact.persisted_variables.where(implicit_key: name).first
            if persisted_var
              if persisted_var.value != cell
                persisted_var.value = cell
                persisted_var.save!
                updated = true unless created
              end
            else
              contact.persisted_variables.create! implicit_key: name, value: cell
              updated = true
            end
          else
            variable = project_variables_by_name[name]
            unless variable
              variable = project.project_variables.create! name: name
              project_variables_by_name[name] = variable
            end

            persisted_var = contact.persisted_variables.where(project_variable_id: variable.id).first
            if persisted_var
              if persisted_var.value != cell
                persisted_var.value = cell
                persisted_var.save!
                udpated = true
              end
            else
              contact.persisted_variables.create! project_variable_id: variable.id, value: cell
              updated = true
            end
          end
        end
      end

      if created
        created_count += 1
      elsif updated
        updated_count += 1
      else
        unchanged_count += 1
      end
    end

    {created: created_count, updated: updated_count, unchanged: unchanged_count}
  end

  def implicit_variables
    ImplicitVariable.subclasses
  end

  def find_implicit_variable(name)
    implicit_variables.find { |implicit| implicit.key.downcase == name.downcase }
  end

  def find_project_variable(name)
    @project_variables.find { |var| var.name.downcase == name.downcase }
  end

  def read_csv
    if @rows.blank?
      CSV.foreach(csv_filename) do |row|
        @rows << row
      end
    end
    @rows
  end

  def csv_filename
    "#{TmpDir}/#{@account.id}-#{@project.id}.csv"
  end
end
