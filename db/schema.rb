# encoding: UTF-8
# This file is auto-generated from the current state of the database. Instead
# of editing this file, please use the migrations feature of Active Record to
# incrementally modify your database, and then regenerate this schema definition.
#
# Note that this schema.rb definition is the authoritative source for your
# database schema. If you need to create the application database on another
# system, you should be using db:schema:load, not running all the migrations
# from scratch. The latter is a flawed and unsustainable approach (the more migrations
# you'll amass, the slower it'll run and the greater likelihood for issues).
#
# It's strongly recommended to check this file into your version control system.

ActiveRecord::Schema.define(:version => 20171211152836) do

  create_table "accounts", :force => true do |t|
    t.string   "email",                               :default => "", :null => false
    t.string   "encrypted_password",   :limit => 128, :default => "", :null => false
    t.string   "reset_password_token"
    t.string   "remember_token"
    t.datetime "remember_created_at"
    t.integer  "sign_in_count",                       :default => 0
    t.datetime "current_sign_in_at"
    t.datetime "last_sign_in_at"
    t.string   "current_sign_in_ip"
    t.string   "last_sign_in_ip"
    t.string   "password_salt"
    t.datetime "created_at",                                          :null => false
    t.datetime "updated_at",                                          :null => false
    t.string   "confirmation_token"
    t.datetime "confirmed_at"
    t.datetime "confirmation_sent_at"
  end

  add_index "accounts", ["confirmation_token"], :name => "index_accounts_on_confirmation_token", :unique => true
  add_index "accounts", ["email"], :name => "index_accounts_on_email", :unique => true
  add_index "accounts", ["reset_password_token"], :name => "index_accounts_on_reset_password_token", :unique => true

  create_table "alerts", :force => true do |t|
    t.integer  "account_id"
    t.string   "type"
    t.string   "severity"
    t.string   "message"
    t.string   "key"
    t.text     "data"
    t.datetime "created_at", :null => false
    t.datetime "updated_at", :null => false
  end

  add_index "alerts", ["account_id", "key"], :name => "index_alerts_on_account_id_and_key"

  create_table "call_flow_external_services", :force => true do |t|
    t.integer  "call_flow_id"
    t.integer  "external_service_id"
    t.datetime "created_at",          :null => false
    t.datetime "updated_at",          :null => false
  end

  add_index "call_flow_external_services", ["call_flow_id"], :name => "index_call_flow_external_services_on_call_flow_id"
  add_index "call_flow_external_services", ["external_service_id"], :name => "index_call_flow_external_services_on_external_service_id"

  create_table "call_flows", :force => true do |t|
    t.string   "name"
    t.binary   "flow"
    t.binary   "user_flow"
    t.string   "callback_url"
    t.integer  "project_id"
    t.text     "encrypted_config"
    t.datetime "created_at",              :null => false
    t.datetime "updated_at",              :null => false
    t.string   "mode"
    t.text     "variables"
    t.string   "fusion_table_name"
    t.string   "current_fusion_table_id"
    t.boolean  "store_in_fusion_tables"
    t.text     "resource_guids"
    t.binary   "broker_flow"
  end

  add_index "call_flows", ["project_id"], :name => "index_call_flows_on_project_id"

  create_table "call_log_entries", :force => true do |t|
    t.integer  "call_id"
    t.string   "severity"
    t.datetime "created_at", :null => false
    t.datetime "updated_at", :null => false
    t.binary   "details"
  end

  add_index "call_log_entries", ["call_id"], :name => "index_call_log_entries_on_call_id"

  create_table "call_logs", :force => true do |t|
    t.integer  "account_id"
    t.integer  "project_id"
    t.datetime "finished_at"
    t.string   "direction"
    t.string   "address"
    t.string   "state",         :default => "active"
    t.datetime "created_at",                          :null => false
    t.datetime "updated_at",                          :null => false
    t.integer  "channel_id"
    t.datetime "started_at"
    t.integer  "schedule_id"
    t.datetime "not_before"
    t.integer  "call_flow_id"
    t.string   "fail_reason"
    t.string   "pbx_logs_guid"
    t.datetime "not_after"
    t.integer  "contact_id"
    t.string   "fail_details"
    t.string   "fail_code"
  end

  add_index "call_logs", ["account_id", "id"], :name => "index_call_logs_on_account_id_and_id"
  add_index "call_logs", ["call_flow_id"], :name => "index_call_logs_on_call_flow_id"
  add_index "call_logs", ["contact_id"], :name => "index_call_logs_on_contact_id"
  add_index "call_logs", ["project_id"], :name => "index_call_logs_on_project_id"

  create_table "channels", :force => true do |t|
    t.integer  "account_id"
    t.integer  "call_flow_id"
    t.string   "name"
    t.text     "config"
    t.datetime "created_at",                     :null => false
    t.datetime "updated_at",                     :null => false
    t.string   "type"
    t.string   "guid"
    t.boolean  "enabled",      :default => true
  end

  add_index "channels", ["call_flow_id"], :name => "index_channels_on_call_flow_id"

  create_table "contact_addresses", :force => true do |t|
    t.string   "address"
    t.integer  "contact_id"
    t.integer  "project_id"
    t.datetime "created_at", :null => false
    t.datetime "updated_at", :null => false
  end

  add_index "contact_addresses", ["contact_id"], :name => "index_contact_addresses_on_contact_id"
  add_index "contact_addresses", ["project_id", "address"], :name => "index_contact_addresses_on_project_id_and_address", :unique => true
  add_index "contact_addresses", ["project_id"], :name => "index_contact_addresses_on_project_id"

  create_table "contact_scheduled_calls", :force => true do |t|
    t.integer  "contact_id"
    t.integer  "scheduled_call_id"
    t.datetime "last_called_at"
    t.datetime "created_at",        :null => false
    t.datetime "updated_at",        :null => false
  end

  add_index "contact_scheduled_calls", ["contact_id"], :name => "index_contact_scheduled_calls_on_contact_id"
  add_index "contact_scheduled_calls", ["scheduled_call_id"], :name => "index_contact_scheduled_calls_on_scheduled_call_id"

  create_table "contacts", :force => true do |t|
    t.datetime "created_at",       :null => false
    t.datetime "updated_at",       :null => false
    t.boolean  "anonymous"
    t.integer  "project_id"
    t.datetime "last_activity_at"
  end

  add_index "contacts", ["project_id"], :name => "index_contacts_on_project_id"

  create_table "delayed_jobs", :force => true do |t|
    t.integer  "priority",          :default => 0
    t.integer  "attempts",          :default => 0
    t.text     "handler"
    t.text     "last_error"
    t.datetime "run_at"
    t.datetime "locked_at"
    t.datetime "failed_at"
    t.string   "locked_by"
    t.string   "queue"
    t.datetime "created_at",                       :null => false
    t.datetime "updated_at",                       :null => false
    t.integer  "scheduled_call_id"
  end

  add_index "delayed_jobs", ["priority", "run_at"], :name => "delayed_jobs_priority"

  create_table "external_service_steps", :force => true do |t|
    t.string   "name"
    t.string   "display_name"
    t.string   "icon"
    t.string   "kind"
    t.string   "callback_url"
    t.text     "variables"
    t.datetime "created_at",          :null => false
    t.datetime "updated_at",          :null => false
    t.string   "response_type"
    t.text     "response_variables"
    t.string   "guid"
    t.integer  "external_service_id"
    t.text     "script"
    t.text     "session_variables"
    t.boolean  "async"
  end

  add_index "external_service_steps", ["external_service_id"], :name => "index_external_service_steps_on_external_service_id"
  add_index "external_service_steps", ["guid"], :name => "index_external_service_steps_on_guid"

  create_table "external_services", :force => true do |t|
    t.integer  "project_id"
    t.string   "name"
    t.string   "url"
    t.text     "xml"
    t.datetime "created_at",      :null => false
    t.datetime "updated_at",      :null => false
    t.text     "global_settings"
    t.string   "guid"
    t.string   "base_url"
  end

  add_index "external_services", ["guid"], :name => "index_external_services_on_guid"
  add_index "external_services", ["project_id"], :name => "index_external_services_on_project_id"

  create_table "feeds", :force => true do |t|
    t.string   "name"
    t.string   "key"
    t.integer  "project_id"
    t.datetime "created_at", :null => false
    t.datetime "updated_at", :null => false
  end

  add_index "feeds", ["key"], :name => "index_feeds_on_key"
  add_index "feeds", ["project_id"], :name => "index_feeds_on_project_id"

  create_table "flow_results_data_packages", :force => true do |t|
    t.string   "uuid"
    t.integer  "call_flow_id"
    t.datetime "from"
    t.datetime "until"
    t.datetime "created_at",   :null => false
    t.datetime "updated_at",   :null => false
  end

  add_index "flow_results_data_packages", ["call_flow_id"], :name => "index_flow_results_data_packages_on_call_flow_id"
  add_index "flow_results_data_packages", ["uuid"], :name => "index_flow_results_data_packages_on_uuid"

  create_table "hibernated_sessions", :force => true do |t|
    t.string   "session_id"
    t.binary   "data"
    t.datetime "created_at", :null => false
    t.datetime "updated_at", :null => false
  end

  create_table "identities", :force => true do |t|
    t.integer  "account_id"
    t.string   "provider"
    t.string   "token"
    t.datetime "created_at", :null => false
    t.datetime "updated_at", :null => false
  end

  create_table "impersonate_records", :force => true do |t|
    t.integer  "call_flow_id"
    t.integer  "contact_id"
    t.integer  "impersonated_id"
    t.datetime "created_at",      :null => false
    t.datetime "updated_at",      :null => false
  end

  add_index "impersonate_records", ["call_flow_id"], :name => "index_impersonate_records_on_call_flow_id"
  add_index "impersonate_records", ["contact_id"], :name => "index_impersonate_records_on_contact_id"

  create_table "instedd_telemetry_counters", :force => true do |t|
    t.integer "period_id"
    t.string  "bucket"
    t.text    "key_attributes"
    t.integer "count",               :default => 0
    t.string  "key_attributes_hash"
  end

  add_index "instedd_telemetry_counters", ["bucket", "key_attributes_hash", "period_id"], :name => "instedd_telemetry_counters_unique_fields", :unique => true

  create_table "instedd_telemetry_periods", :force => true do |t|
    t.datetime "beginning"
    t.datetime "end"
    t.datetime "stats_sent_at"
    t.datetime "created_at",      :null => false
    t.datetime "updated_at",      :null => false
    t.string   "lock_owner"
    t.datetime "lock_expiration"
  end

  create_table "instedd_telemetry_set_occurrences", :force => true do |t|
    t.integer "period_id"
    t.string  "bucket"
    t.text    "key_attributes"
    t.string  "element"
    t.string  "key_attributes_hash"
  end

  add_index "instedd_telemetry_set_occurrences", ["bucket", "key_attributes_hash", "element", "period_id"], :name => "instedd_telemetry_set_occurrences_unique_fields", :unique => true

  create_table "instedd_telemetry_settings", :force => true do |t|
    t.string "key"
    t.string "value"
  end

  add_index "instedd_telemetry_settings", ["key"], :name => "index_instedd_telemetry_settings_on_key", :unique => true

  create_table "instedd_telemetry_timespans", :force => true do |t|
    t.string   "bucket"
    t.text     "key_attributes"
    t.datetime "since"
    t.datetime "until"
    t.string   "key_attributes_hash"
  end

  add_index "instedd_telemetry_timespans", ["bucket", "key_attributes_hash"], :name => "instedd_telemetry_timespans_unique_fields", :unique => true

  create_table "localized_resources", :force => true do |t|
    t.string   "language"
    t.text     "text"
    t.binary   "recorded_audio", :limit => 2147483647
    t.string   "url"
    t.string   "type"
    t.datetime "created_at",                           :null => false
    t.datetime "updated_at",                           :null => false
    t.text     "extras"
    t.binary   "uploaded_audio", :limit => 2147483647
    t.string   "guid"
    t.integer  "resource_id"
  end

  add_index "localized_resources", ["guid"], :name => "index_localized_resources_on_guid"
  add_index "localized_resources", ["resource_id"], :name => "index_localized_resources_on_resource_id"

  create_table "nuntium_channels", :force => true do |t|
    t.integer  "account_id"
    t.string   "name"
    t.string   "channel_name"
    t.boolean  "enabled",      :default => true
    t.datetime "created_at",                     :null => false
    t.datetime "updated_at",                     :null => false
  end

  add_index "nuntium_channels", ["account_id"], :name => "index_nuntium_channels_on_account_id"

  create_table "o_auth_tokens", :force => true do |t|
    t.integer  "account_id"
    t.string   "service"
    t.string   "access_token"
    t.string   "refresh_token"
    t.datetime "expires_at"
    t.datetime "created_at",    :null => false
    t.datetime "updated_at",    :null => false
  end

  create_table "pbx_logs", :force => true do |t|
    t.string   "guid"
    t.text     "details"
    t.datetime "created_at", :null => false
    t.datetime "updated_at", :null => false
  end

  add_index "pbx_logs", ["guid", "id"], :name => "index_pbx_logs_on_guid_and_id"

  create_table "permissions", :force => true do |t|
    t.integer  "account_id"
    t.string   "type"
    t.integer  "model_id"
    t.string   "role"
    t.datetime "created_at", :null => false
    t.datetime "updated_at", :null => false
  end

  create_table "persisted_variables", :force => true do |t|
    t.string   "value"
    t.datetime "created_at",          :null => false
    t.datetime "updated_at",          :null => false
    t.integer  "contact_id"
    t.integer  "project_variable_id"
    t.string   "implicit_key"
  end

  add_index "persisted_variables", ["contact_id", "implicit_key", "value"], :name => "index_vars_on_contact_id_and_key_and_value"
  add_index "persisted_variables", ["contact_id", "project_variable_id", "value"], :name => "index_vars_on_contact_id_and_var_id_and_value"
  add_index "persisted_variables", ["contact_id"], :name => "index_persisted_variables_on_contact_id"
  add_index "persisted_variables", ["project_variable_id"], :name => "index_persisted_variables_on_project_variable_id"

  create_table "project_variables", :force => true do |t|
    t.integer  "project_id"
    t.string   "name"
    t.datetime "created_at", :null => false
    t.datetime "updated_at", :null => false
  end

  add_index "project_variables", ["project_id"], :name => "index_project_variables_on_project_id"

  create_table "projects", :force => true do |t|
    t.string   "name"
    t.datetime "created_at",                                      :null => false
    t.datetime "updated_at",                                      :null => false
    t.integer  "account_id"
    t.string   "status_callback_url"
    t.text     "encrypted_config"
    t.string   "time_zone",                    :default => "UTC"
    t.text     "languages"
    t.string   "default_language"
    t.boolean  "status_callback_include_vars", :default => false
  end

  create_table "queued_calls", :force => true do |t|
    t.integer  "channel_id"
    t.integer  "call_log_id"
    t.string   "address"
    t.datetime "created_at",                         :null => false
    t.datetime "updated_at",                         :null => false
    t.string   "callback_url"
    t.binary   "flow"
    t.string   "status_callback_url"
    t.integer  "schedule_id"
    t.datetime "not_before"
    t.integer  "retries",             :default => 0
    t.integer  "project_id"
    t.integer  "call_flow_id"
    t.string   "time_zone"
    t.text     "variables"
    t.string   "session_id"
    t.text     "callback_params"
    t.datetime "not_after"
    t.integer  "contact_id"
    t.integer  "scheduled_call_id"
  end

  add_index "queued_calls", ["call_flow_id"], :name => "index_queued_calls_on_call_flow_id"
  add_index "queued_calls", ["call_log_id"], :name => "index_queued_calls_on_call_log_id"
  add_index "queued_calls", ["project_id"], :name => "index_queued_calls_on_application_id"

  create_table "recorded_audios", :force => true do |t|
    t.integer  "contact_id"
    t.integer  "call_log_id"
    t.string   "key"
    t.string   "description"
    t.datetime "created_at",  :null => false
    t.datetime "updated_at",  :null => false
    t.integer  "project_id"
  end

  add_index "recorded_audios", ["call_log_id"], :name => "index_recorded_audios_on_call_log_id"
  add_index "recorded_audios", ["contact_id"], :name => "index_recorded_audios_on_contact_id"
  add_index "recorded_audios", ["project_id", "created_at"], :name => "index_recorded_audios_on_project_id_and_created_at"

  create_table "resources", :force => true do |t|
    t.string   "name"
    t.integer  "project_id"
    t.datetime "created_at", :null => false
    t.datetime "updated_at", :null => false
    t.string   "guid"
  end

  add_index "resources", ["guid"], :name => "index_resources_on_guid"
  add_index "resources", ["project_id"], :name => "index_resources_on_project_id"

  create_table "scheduled_calls", :force => true do |t|
    t.string   "name"
    t.boolean  "enabled",            :default => true
    t.integer  "project_id"
    t.integer  "call_flow_id"
    t.integer  "channel_id"
    t.boolean  "not_before_enabled", :default => false
    t.datetime "not_before"
    t.string   "time_zone"
    t.text     "filters"
    t.datetime "created_at",                            :null => false
    t.datetime "updated_at",                            :null => false
    t.boolean  "not_after_enabled",  :default => false
    t.datetime "not_after"
    t.integer  "from_time"
    t.integer  "to_time"
    t.text     "recurrence"
  end

  add_index "scheduled_calls", ["call_flow_id"], :name => "index_scheduled_calls_on_call_flow_id"
  add_index "scheduled_calls", ["channel_id"], :name => "index_scheduled_calls_on_channel_id"
  add_index "scheduled_calls", ["project_id"], :name => "index_scheduled_calls_on_project_id"

  create_table "schedules", :force => true do |t|
    t.string   "name"
    t.string   "retries"
    t.time     "time_from"
    t.time     "time_to"
    t.string   "weekdays"
    t.datetime "created_at", :null => false
    t.datetime "updated_at", :null => false
    t.integer  "project_id"
  end

  add_index "schedules", ["project_id"], :name => "index_schedules_on_project_id"

  create_table "traces", :force => true do |t|
    t.integer  "call_flow_id"
    t.integer  "call_id"
    t.string   "result"
    t.datetime "created_at",   :null => false
    t.datetime "updated_at",   :null => false
    t.string   "step_name"
    t.string   "step_id"
  end

  add_index "traces", ["call_flow_id"], :name => "index_traces_on_call_flow_id"

end
