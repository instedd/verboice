class ChangeProjectsUserFlowToBinary < ActiveRecord::Migration
  def up
    rename_column :projects, :user_flow, :text_user_flow
    add_column    :projects, :user_flow, :binary

    connection.select_rows("SELECT id, text_user_flow FROM projects").each do |id, text_user_flow|
      begin
        unless text_user_flow.blank?
          user_flow = YAML.load(text_user_flow)
          binary_user_flow = Zlib.deflate(Marshal.dump(user_flow), 9)
          connection.execute("UPDATE projects SET user_flow=x'#{binary_user_flow.unpack("H*")[0]}' WHERE id=#{id}")
        end
      rescue Exception => ex
        $stderr.puts "Error migrating flow for project #{id}: #{ex.message}\nFlow was: #{text_user_flow}\n#{ex.backtrace}\n"
      end
    end

    remove_column :projects, :text_user_flow
  end

  def down
    rename_column :projects, :user_flow, :binary_user_flow
    add_column    :projects, :user_flow, :text

    connection.select_rows("SELECT id, binary_user_flow FROM projects").each do |id, binary_user_flow|
      begin
        unless binary_user_flow.blank?
          user_flow = Marshal.load(Zlib.inflate(binary_user_flow)) rescue nil
          text_user_flow = YAML.dump(user_flow)
          sanitized_text_user_flow_assignment = ActiveRecord::Base.send(:sanitize_sql_for_assignment, :user_flow => text_user_flow)
          connection.execute("UPDATE projects SET #{sanitized_text_user_flow_assignment} WHERE id=#{id}")
        end
      rescue Exception => ex
        $stderr.puts "Error reversing migration for flow for project #{id}: #{ex.message}\nFlow was: #{binary_user_flow}\n#{ex.backtrace}\n"
      end
    end

    remove_column :projects, :binary_user_flow
  end
end
