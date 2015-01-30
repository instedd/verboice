class MigrateContactIdInCallLogs < ActiveRecord::Migration
  def up
    last_id = 0

    loop do
      call_logs = execute "SELECT id, project_id, address FROM call_logs "\
                          "WHERE contact_id IS NULL AND id > #{last_id} "\
                          "ORDER BY id ASC LIMIT 5000"
      call_logs = call_logs.to_a

      break if call_logs.empty?

      last_id = call_logs[-1][0]

      call_logs.each do |call_log|
        call_log_id = call_log[0]
        project_id = call_log[1]
        address = call_log[2]

        next unless project_id.present?

        contact_addresses = execute "SELECT contact_id FROM contact_addresses "\
                                    "WHERE project_id = #{project_id} "\
                                    "AND address = '#{address}' "\
                                    "LIMIT 1"
        contact_addresses = contact_addresses.to_a

        unless contact_addresses.empty?
          contact_id = contact_addresses[0][0]

          execute "UPDATE call_logs SET contact_id = #{contact_id} WHERE id = #{call_log_id}"
        end
      end
    end
  end

  def down
    execute "UPDATE call_logs SET contact_id = NULL"
  end
end
