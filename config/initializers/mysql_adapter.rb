ActiveSupport.on_load(:active_record) do
  class ActiveRecord::ConnectionAdapters::Mysql2Adapter
    NATIVE_DATABASE_TYPES[:primary_key] = "int(11) auto_increment PRIMARY KEY"
  end
end
