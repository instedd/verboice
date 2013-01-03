# Copyright (C) 2010-2012, InSTEDD
#
# This file is part of Verboice.
#
# Verboice is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Verboice is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Verboice.  If not, see <http://www.gnu.org/licenses/>.

module CallFlow::FusionTablesPush

  def push_to_fusion_tables(call_log)
    Delayed::Job.enqueue Pusher.new(self.id, call_log.id)
  end

  class Pusher < Struct.new(:call_flow_id, :call_log_id)

    API_URL = "https://www.googleapis.com/fusiontables/v1/query"

    attr_accessor :call_flow, :call_log, :access_token

    delegate :current_fusion_table_id, :fusion_table_name, to: :call_flow

    def perform
      self.call_flow = CallFlow.find(self.call_flow_id)
      self.call_log = CallLog.find(self.call_log_id)
      return if !OAuth2::Client.service_configured?(:google) || !self.call_flow.store_in_fusion_tables || self.call_flow.fusion_table_name.blank? || self.call_flow.account.google_oauth_token.nil?
      push
    end

    def push
      load_token
      create_table unless has_table? && is_table_valid?
      upload_call_data
    end

    def load_token
      self.access_token = call_flow.account.google_oauth_token.tap do |t|
        t.refresh! && t.save! if t.expired?
      end.access_token
    end

    def upload_call_data
      columns_expr = columns.map{|name, kind| "'#{name.gsub("'", "\\'")}'"}.join(', ')

      ids = call_flow.step_names.keys
      values = [call_log.id, call_log.address, call_log.state, call_log.started_at, call_log.finished_at]

      call_log.traces.each do |trace|
        values[ids.index(trace.step_id.to_i) + 5] = trace.result rescue nil
      end

      columns.count.times {|i| values[i] ||= '' }
      values_expr = values.map{|val| "'#{val}'"}.join(', ')

      query = "INSERT INTO #{current_fusion_table_id} ( #{columns_expr} ) VALUES ( #{values_expr} )"
      post_sql_query query
    end

    def create_table
      columns_expr = columns.map{|name, kind| "'#{name.gsub("'", "\\'")}': #{kind || 'STRING'}"}.join(', ')
      query = "CREATE TABLE #{new_table_name} ( #{columns_expr} )"
      response = post_sql_query query
      id = csv_parse(response)[:tableid][0]
      call_flow.update_attribute :current_fusion_table_id, id
    end

    def has_table?
      not current_fusion_table_id.blank?
    end

    def new_table_name
      existing = list_tables.map{|r| r[:name]}
      index = 1
      while existing.include?(make_name(index)) do
        index += 1
      end
      make_name(index)
    end

    def make_name(index)
      "#{fusion_table_name.strip.gsub(/ /,'_')}_#{index.to_s.rjust(3,'0')}"
    end

    def is_table_valid?
      current_columns = get_table_columns(current_fusion_table_id).map{|row| row[:name]}
      expected_columns = columns_names()
      return current_columns[0...expected_columns.length] == expected_columns
    rescue Exception => ex
      return false
    end

    def list_tables
      csv_parse get_sql_query("SHOW TABLES")
    end

    def get_table_columns(table_id)
      csv_parse get_sql_query("DESCRIBE #{table_id}")
    end

    private

    def post_sql_query(query)
      RestClient.post API_URL, {:sql => query, :alt => 'csv'}, {:params => {:access_token => access_token}}
    end

    def get_sql_query(query)
      RestClient.get API_URL, {:params => {:sql => query, :alt => 'csv', :access_token => access_token}}
    end

    def csv_parse(csv)
      CSV.parse csv, {:headers => true, :header_converters => :symbol}
    end

    def columns
      step_names = []
      call_flow.step_names.values.map(&:to_s).each do |original_step_name|
        step_name = original_step_name
        index = 1
        while step_names.include?(step_name)
          index += 1
          step_name = "#{original_step_name}_#{index}"
        end
        step_names << step_name
      end

      ['Call ID', 'Phone Number', 'State', ['Start Time', 'DATETIME'], ['End Time', 'DATETIME']] + step_names
    end

    def columns_names
      columns.map{|name,type| name}
    end

  end

end
