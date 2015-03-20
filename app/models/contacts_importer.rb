class ContactsImporter
  TmpDir = "#{Rails.root}/tmp"
  PhoneRegex = /phone|number|phone number/i

  attr_reader :account, :project
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
        {action: 'ignore', name: col}
      when PhoneRegex
        {action: 'phone_number', name: 'Phone number'}
      else
        implicit_variable = find_implicit_variable(col)
        if implicit_variable
          {action: 'existing_variable', name: implicit_variable.key}
        else
          project_variable = find_project_variable(col)
          if project_variable
            {action: 'existing_variable', name: project_variable.name, project_variable: project_variable.id}
          else
            {action: 'new_field', name: col}
          end
        end
      end
    end
  end

  def find_implicit_variable(name)
    ImplicitVariable.subclasses.find { |implicit| implicit.key.downcase == name.downcase }
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
