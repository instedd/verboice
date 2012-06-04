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

ActiveRecord::Schema.define(:version => 20120601200223) do

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
    t.datetime "created_at",       :null => false
    t.datetime "updated_at",       :null => false
    t.string   "mode"
  end

  add_index "call_flows", ["project_id"], :name => "index_call_flows_on_project_id"

  create_table "call_log_entries", :force => true do |t|
    t.integer  "call_id"
    t.string   "step_name"
    t.string   "step_id"
    t.text     "description"
    t.string   "severity"
    t.datetime "created_at",  :null => false
    t.datetime "updated_at",  :null => false
  end

  add_index "call_log_entries", ["call_id"], :name => "index_call_log_entries_on_call_id"

  create_table "call_logs", :force => true do |t|
    t.integer  "account_id"
    t.integer  "project_id"
    t.datetime "finished_at"
    t.string   "direction"
    t.string   "address"
    t.string   "state",        :default => "active"
    t.text     "details"
    t.datetime "created_at",                         :null => false
    t.datetime "updated_at",                         :null => false
    t.integer  "channel_id"
    t.datetime "started_at"
    t.integer  "schedule_id"
    t.datetime "not_before"
    t.integer  "call_flow_id"
  end

  add_index "call_logs", ["call_flow_id"], :name => "index_call_logs_on_call_flow_id"

  create_table "channels", :force => true do |t|
    t.integer  "account_id"
    t.integer  "call_flow_id"
    t.string   "name"
    t.text     "config"
    t.datetime "created_at",   :null => false
    t.datetime "updated_at",   :null => false
    t.string   "kind"
    t.string   "guid"
  end

  add_index "channels", ["call_flow_id"], :name => "index_channels_on_call_flow_id"

  create_table "contacts", :force => true do |t|
    t.string   "address"
    t.integer  "account_id"
    t.datetime "created_at", :null => false
    t.datetime "updated_at", :null => false
    t.boolean  "anonymous"
  end

  create_table "external_services", :force => true do |t|
    t.integer  "project_id"
    t.string   "name"
    t.string   "url"
    t.text     "xml"
    t.datetime "created_at", :null => false
    t.datetime "updated_at", :null => false
  end

  add_index "external_services", ["project_id"], :name => "index_external_services_on_project_id"

  create_table "persisted_variables", :force => true do |t|
    t.string   "value"
    t.string   "name"
    t.datetime "created_at", :null => false
    t.datetime "updated_at", :null => false
    t.integer  "contact_id"
  end

  add_index "persisted_variables", ["contact_id"], :name => "index_persisted_variables_on_contact_id"

  create_table "projects", :force => true do |t|
    t.string   "name"
    t.datetime "created_at",                             :null => false
    t.datetime "updated_at",                             :null => false
    t.integer  "account_id"
    t.string   "status_callback_url"
    t.text     "encrypted_config"
    t.string   "time_zone",           :default => "UTC"
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

  create_table "schedules", :force => true do |t|
    t.integer  "account_id"
    t.string   "name"
    t.string   "retries"
    t.time     "time_from"
    t.time     "time_to"
    t.string   "weekdays"
    t.datetime "created_at", :null => false
    t.datetime "updated_at", :null => false
  end

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
