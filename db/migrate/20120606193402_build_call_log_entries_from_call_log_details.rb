class BuildCallLogEntriesFromCallLogDetails < ActiveRecord::Migration
  def up
    levels = {'E' => :error, 'W' => :warn, 'I' => :info, 'T' => :trace}

    connection.execute("SELECT id, details FROM call_logs").each do |id, details|

      if details
        lines = details.split("\n")
        str = []
        last = nil
        lines.each do |line|
          if line.match /(E|W|I|T) (\d+(?:\.\d+)) (.*)/
            last = {:severity => levels[$1], :time => Time.at($2.to_f).utc, :text => $3}
            str << last
          else
            last[:text] << "\n#{line}"
          end
        end

        str.each do |log_entry|
          sanitized_assignment = ActiveRecord::Base.send(
            :sanitize_sql_for_assignment,
            description: log_entry[:text],
            call_id: id,
            severity: log_entry[:severity],
            created_at: log_entry[:time].strftime("%F %T")
          )

          connection.execute("INSERT INTO call_log_entries SET #{sanitized_assignment}")
        end
      end
    end

    remove_column :call_logs, :details
  end

  def down
    add_column :call_logs, :details, :text

    levels = {:error => 'E', :warn => 'W', :info => 'I', :trace => 'T'}

    connection.execute("SELECT id FROM call_logs").each do |id|
      details = ""
      connection.execute("SELECT created_at, severity, description FROM call_log_entries WHERE call_id = #{id.first}").each do |created_at, severity, description|
        details += "#{levels[severity.to_sym]} #{created_at.utc.to_f} #{description}\n"
      end
      sanitized_assignment = ActiveRecord::Base.send(
        :sanitize_sql_for_assignment,
        details: details)

      connection.execute("UPDATE call_logs SET #{sanitized_assignment} WHERE id=#{id.first}")
    end
  end
end
