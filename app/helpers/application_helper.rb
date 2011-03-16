module ApplicationHelper
  def verbo_version
    begin
      @@verbo_version = File.read('VERSION').strip unless defined? @@verbo_version
    rescue Errno::ENOENT
      @@verbo_version = 'Development'
    end
    @@verbo_version
  end
end
