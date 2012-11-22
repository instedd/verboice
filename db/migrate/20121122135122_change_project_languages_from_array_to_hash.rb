class ChangeProjectLanguagesFromArrayToHash < ActiveRecord::Migration
  class Project < ActiveRecord::Base
    serialize :languages
  end

  def up
    Project.all.each do |project|
      if project.languages.present?
        project.languages = project.languages.map do |lang|
          if lang.is_a?(String)
            {'language' => lang}
          else
            lang
          end
        end
        project.save!
      end
    end
  end

  def down
    Project.all.each do |project|
      if project.languages.present?
        project.languages = project.languages.map do |lang|
          if lang.is_a?(Hash)
            lang['language']
          else
            lang
          end
        end
        project.save!
      end
    end
  end
end
