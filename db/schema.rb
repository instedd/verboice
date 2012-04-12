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

ActiveRecord::Schema.define(:version => 20120411195812) do

  create_table "accounts", :force => true do |t|
    t.string   "email",                               :default => "", :null => false
    t.string   "encrypted_password",   :limit => 128, :default => "", :null => false
    t.string   "password_salt",                       :default => "", :null => false
    t.string   "reset_password_token"
    t.string   "remember_token"
    t.datetime "remember_created_at"
    t.integer  "sign_in_count",                       :default => 0
    t.datetime "current_sign_in_at"
    t.datetime "last_sign_in_at"
    t.string   "current_sign_in_ip"
    t.string   "last_sign_in_ip"
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  add_index "accounts", ["email"], :name => "index_accounts_on_email", :unique => true
  add_index "accounts", ["reset_password_token"], :name => "index_accounts_on_reset_password_token", :unique => true

  create_table "applications", :force => true do |t|
    t.string   "name"
    t.string   "callback_url"
    t.text     "flow"
    t.datetime "created_at"
    t.datetime "updated_at"
    t.integer  "account_id"
    t.string   "status_callback_url"
    t.text     "encrypted_config"
    t.text     "user_flow"
  end

  create_table "call_logs", :force => true do |t|
    t.integer  "account_id"
    t.integer  "application_id"
    t.datetime "finished_at"
    t.string   "direction"
    t.string   "address"
    t.string   "state",          :default => "active"
    t.text     "details"
    t.datetime "created_at"
    t.datetime "updated_at"
    t.integer  "channel_id"
    t.datetime "started_at"
    t.integer  "call_queue_id"
    t.datetime "not_before"
  end

  create_table "call_queues", :force => true do |t|
    t.integer  "account_id"
    t.string   "name"
    t.string   "retries"
    t.time     "time_from"
    t.time     "time_to"
    t.string   "weekdays"
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  create_table "channels", :force => true do |t|
    t.integer  "account_id"
    t.integer  "application_id"
    t.string   "name"
    t.text     "config"
    t.datetime "created_at"
    t.datetime "updated_at"
    t.string   "kind"
  end

  create_table "queued_calls", :force => true do |t|
    t.integer  "channel_id"
    t.integer  "call_log_id"
    t.string   "address"
    t.datetime "created_at"
    t.datetime "updated_at"
    t.string   "callback_url"
    t.text     "flow"
    t.string   "status_callback_url"
    t.integer  "call_queue_id"
    t.datetime "not_before"
    t.integer  "retries",             :default => 0
  end

  create_table "traces", :force => true do |t|
    t.integer  "application_id"
    t.integer  "step_id"
    t.integer  "call_id"
    t.string   "result"
    t.datetime "created_at"
    t.datetime "updated_at"
    t.string   "step_name"
  end

  add_index "traces", ["application_id"], :name => "index_traces_on_application_id"

end
