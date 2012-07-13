class AddLanguagesToProjects < ActiveRecord::Migration
  def change
    add_column :projects, :languages, :text
    add_column :projects, :default_language, :string
  end
end
