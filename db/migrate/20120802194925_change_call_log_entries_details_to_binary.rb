class ChangeCallLogEntriesDetailsToBinary < ActiveRecord::Migration
  def up
    rename_column :call_log_entries, :details, :text_details
    add_column    :call_log_entries, :details, :binary

    connection.select_rows("SELECT id, text_details FROM call_log_entries").each do |id, text_details|
      begin
        unless text_details.blank?
          details = YAML.load(text_details)
          binary_details = Zlib.deflate(Marshal.dump(details), 9)
          connection.execute("UPDATE call_log_entries SET details=x'#{binary_details.unpack("H*")[0]}' WHERE id=#{id}")
        end
      rescue Exception => ex
        $stderr.puts "Error migrating details for call log entry #{id}: #{ex.message}\nDetails were: #{text_details}\n#{ex.backtrace}\n"
      end
    end

    remove_column :call_log_entries, :text_details
  end

  def down
    rename_column :call_log_entries, :details, :binary_details
    add_column    :call_log_entries, :details, :text

    connection.select_rows("SELECT id, binary_details FROM call_log_entries").each do |id, binary_details|
      begin
        unless binary_details.blank?
          details = Marshal.load(Zlib.inflate(binary_details)) rescue nil
          text_details = YAML.dump(details)
          sanitized_text_details_assignment = ActiveRecord::Base.send(:sanitize_sql_for_assignment, :details => text_details)
          connection.execute("UPDATE call_log_entries SET #{sanitized_text_details_assignment} WHERE id=#{id}")
        end
      rescue Exception => ex
        $stderr.puts "Error reversing migration for details for call log entry #{id}: #{ex.message}\nDetails were: #{binary_details}\n#{ex.backtrace}\n"
      end
    end

    remove_column :call_log_entries, :binary_details
  end
end
