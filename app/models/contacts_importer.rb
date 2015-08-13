class ContactsImporter

  TmpDir = "#{Rails.root}/tmp"
  PhoneRegex = /phone|number|phone number/i
  DefaultIgnoredColumns = ["First call", "Last Call Timestamp", "Last Call Callflow Name", "Last Successful Call Timestamp", "Last Successful Call CallFlow Name", "Last Used Channel"]

  attr_reader :account, :project, :project_variables
  attr_accessor :rows_preview, :column_specs

  def initialize(account, project)
    @account = account
    @project = project
    @project_variables = project.project_variables.all
    @rows_preview = []
    @column_specs = []
  end

  def save_csv(file_param)

    if file_param.blank?
      return "You didn't specify a CSV file"
    end

    if file_param.size == 0
      return "The CSV file must not be empty"
    end

    FileUtils.move file_param.tempfile.path, csv_filename

    begin
      @rows_preview = read_preview
    rescue => ex
      return "Error reading CSV: #{ex}"
    end

    if @rows_preview.empty?
      return "The CSV file is empty"
    end

    unless @rows_preview.all? {|row| row.size == @rows_preview[0].size}
      return "All rows in the CSV must have the same number of columns"
    end

    nil
  end

  def guess_column_specs
    headers = @rows_preview.first
    headers.map do |col|
      col = col.strip

      case col
      when ""
        {action: 'ignore', name: col, id: '', source: col}
      when *DefaultIgnoredColumns
        {action: 'ignore', name: col, id: '', source: col}
      when PhoneRegex
        {action: 'phone_number', name: 'Phone number', id: '', source: col}
      else
        implicit_variable = find_implicit_variable(col)
        if implicit_variable
          {action: 'existing_variable', name: implicit_variable.key, id: implicit_variable.key, source: col}
        else
          project_variable = find_project_variable(col)
          if project_variable
            {action: 'existing_variable', name: project_variable.name, id: project_variable.id, source: col}
          else
            {action: 'new_variable', name: col, id: '', source: col}
          end
        end
      end
    end
  end

  def import
    page = []
    headers = nil
    total_counts = {created: 0, updated: 0, unchanged: 0}

    # Import CSV in 'pages' of 100 items
    CSV.foreach(csv_filename) do |row|
      headers = row and next if headers.nil?

      page << row
      if page.size >= csv_page_size
        update_counts(total_counts, page)
        page = []
      end
    end

    update_counts(total_counts, page) if not page.empty?
    total_counts
  end

  def update_counts(total_counts, page)
    Contact.transaction do
      page_counts = import_page(page)
      total_counts.keys.each do |key|
        total_counts[key] += page_counts[key]
      end
    end
  end

  def import_page(rows)

    # Check where is the phone column
    phone_number_index = @column_specs.index { |spec| spec['action'] == 'phone_number' }

    # Gather all phone numbers
    phones = rows.map do |row|
      phone = row[phone_number_index].strip

      # If it looks like a phone number (could be a sip address), removing extraneous chars
      phone = phone.gsub /\s|\(|\)|\+|\-/, "" if phone =~ /(\d|\s|\(|\)|\+|\-)+/

      # Update the row so later we don't have to do this again
      row[phone_number_index] = phone

      phone
    end

    # Find all contacts address with those numbers
    contact_addresses = ContactAddress.where(project_id: @project.id, address: phones).includes(contact: :persisted_variables).all

    # And index them by their address, so we can look them up faster
    contact_addresses_by_address = contact_addresses.index_by &:address

    project_variables_by_name = project_variables.index_by &:name

    created_count = 0
    updated_count = 0
    unchanged_count = 0

    # Now process all rows
    rows.each do |row|
      phone = row[phone_number_index]

      # Skip if phone is empty
      next if phone.blank?

      # Find contact address or create one (and its associated contact)
      contact_address = contact_addresses_by_address[phone]
      if contact_address
        contact = contact_address.contact
        created = false
      else
        contact = project.contacts.new
        contact_address = contact.addresses.new address: phone
        contact_address.project_id = project.id
        contact.save!(validate: false) # Skip uniqueness validation and delegate to unique index in DB

        contact_addresses_by_address[phone] = contact_address
        created = true
      end

      updated = false

      # Process all cells according to spec
      row.each_with_index do |cell, index|
        cell = cell.strip if cell

        spec = @column_specs[index]
        case spec['action']
        when 'new_variable', 'existing_variable'
          name = spec['name']

          implicit_variable = implicit_variables.find { |var| var.key == name }
          if implicit_variable
            persisted_var = contact.persisted_variables.find{|var| var.implicit_key == name}
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
              project_variables << variable
            end

            persisted_var = contact.persisted_variables.find{|var| var.project_variable_id == variable.id}
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

  def read_preview(n=20)
    preview = []
    CSV.foreach(csv_filename) do |row|
      preview << row
      break if preview.size >= n
    end

    preview
  end

  def csv_filename
    "#{TmpDir}/#{@account.id}-#{@project.id}.csv"
  end

  def csv_page_size
    100
  end
end
