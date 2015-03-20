class ContactsImporter
  def initialize(account, project)
    @account = account
    @project = project
  end

  def save_csv(file)
    true
  end

  def guess_column_specs
  end
end
