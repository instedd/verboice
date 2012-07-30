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

ActiveRecord::Schema.define(:version => 20120730180843) do

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
    t.text     "external_service_guids"
    t.string   "fusion_table_name"
    t.string   "current_fusion_table_id"
    t.boolean  "store_in_fusion_tables"
  end

  add_index "call_flows", ["project_id"], :name => "index_call_flows_on_project_id"

  create_table "call_log_entries", :force => true do |t|
    t.integer  "call_id"
    t.text     "details"
    t.string   "severity"
    t.datetime "created_at", :null => false
    t.datetime "updated_at", :null => false
  end

  add_index "call_log_entries", ["call_id"], :name => "index_call_log_entries_on_call_id"

  create_table "call_logs", :force => true do |t|
    t.integer  "account_id"
    t.integer  "project_id"
    t.datetime "finished_at"
    t.string   "direction"
    t.string   "address"
    t.string   "state",        :default => "active"
    t.datetime "created_at",                         :null => false
    t.datetime "updated_at",                         :null => false
    t.integer  "channel_id"
    t.datetime "started_at"
    t.integer  "schedule_id"
    t.datetime "not_before"
    t.integer  "call_flow_id"
    t.string   "fail_reason"
  end

  add_index "call_logs", ["call_flow_id"], :name => "index_call_logs_on_call_flow_id"

  create_table "channels", :force => true do |t|
    t.integer  "account_id"
    t.integer  "call_flow_id"
    t.string   "name"
    t.text     "config"
    t.datetime "created_at",   :null => false
    t.datetime "updated_at",   :null => false
    t.string   "type"
    t.string   "guid"
  end

  add_index "channels", ["call_flow_id"], :name => "index_channels_on_call_flow_id"

  create_table "contacts", :force => true do |t|
    t.string   "address"
    t.datetime "created_at", :null => false
    t.datetime "updated_at", :null => false
    t.boolean  "anonymous"
    t.integer  "project_id"
  end

  add_index "contacts", ["project_id"], :name => "index_contacts_on_project_id"

  create_table "delayed_jobs", :force => true do |t|
    t.integer  "priority",   :default => 0
    t.integer  "attempts",   :default => 0
    t.text     "handler"
    t.text     "last_error"
    t.datetime "run_at"
    t.datetime "locked_at"
    t.datetime "failed_at"
    t.string   "locked_by"
    t.string   "queue"
    t.datetime "created_at",                :null => false
    t.datetime "updated_at",                :null => false
  end

  add_index "delayed_jobs", ["priority", "run_at"], :name => "delayed_jobs_priority"

  create_table "external_service_steps", :force => true do |t|
    t.string   "name"
    t.string   "display_name"
    t.string   "icon"
    t.string   "kind"
    t.string   "callback_url"
    t.text     "variables"
    t.datetime "created_at",            :null => false
    t.datetime "updated_at",            :null => false
    t.string   "response_type"
    t.text     "response_variables"
    t.string   "guid"
    t.string   "external_service_guid"
  end

  add_index "external_service_steps", ["external_service_guid"], :name => "index_external_service_steps_on_external_service_guid"
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
  end

  add_index "external_services", ["guid"], :name => "index_external_services_on_guid"
  add_index "external_services", ["project_id"], :name => "index_external_services_on_project_id"

  create_table "localized_resources", :force => true do |t|
    t.string   "language"
    t.string   "text"
    t.binary   "audio"
    t.string   "url"
    t.string   "type"
    t.integer  "resource_id"
    t.datetime "created_at",  :null => false
    t.datetime "updated_at",  :null => false
    t.text     "extras"
  end

  add_index "localized_resources", ["resource_id"], :name => "index_localized_resources_on_resource_id"

  create_table "o_auth_tokens", :force => true do |t|
    t.integer  "account_id"
    t.string   "service"
    t.string   "access_token"
    t.string   "refresh_token"
    t.datetime "expires_at"
    t.datetime "created_at",    :null => false
    t.datetime "updated_at",    :null => false
  end

  create_table "persisted_variables", :force => true do |t|
    t.string   "value"
    t.datetime "created_at",          :null => false
    t.datetime "updated_at",          :null => false
    t.integer  "contact_id"
    t.integer  "project_variable_id"
    t.string   "implicit_key"
  end

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
    t.datetime "created_at",                             :null => false
    t.datetime "updated_at",                             :null => false
    t.integer  "account_id"
    t.string   "status_callback_url"
    t.text     "encrypted_config"
    t.string   "time_zone",           :default => "UTC"
    t.text     "languages"
    t.string   "default_language"
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
  end

  add_index "queued_calls", ["call_flow_id"], :name => "index_queued_calls_on_call_flow_id"
  add_index "queued_calls", ["project_id"], :name => "index_queued_calls_on_application_id"

  create_table "recorded_audios", :force => true do |t|
    t.integer  "contact_id"
    t.integer  "call_log_id"
    t.string   "key"
    t.string   "description"
    t.datetime "created_at",  :null => false
    t.datetime "updated_at",  :null => false
  end

  add_index "recorded_audios", ["call_log_id"], :name => "index_recorded_audios_on_call_log_id"
  add_index "recorded_audios", ["contact_id"], :name => "index_recorded_audios_on_contact_id"

  create_table "resources", :force => true do |t|
    t.string   "name"
    t.integer  "project_id"
    t.datetime "created_at", :null => false
    t.datetime "updated_at", :null => false
  end

  add_index "resources", ["project_id"], :name => "index_resources_on_project_id"

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
