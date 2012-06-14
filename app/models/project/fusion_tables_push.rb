module Project::FusionTablesPush

  def push_to_fusion_tables(call_log)
    return if fusion_table_name.blank?
    Pusher.new(self, call_log).push
  end

  class Pusher

    API_URL = "https://www.google.com/fusiontables/api/query"

    attr_accessor :project, :call_log

    delegate :current_fusion_table_id, :fusion_table_name, to: :project

    def initialize(project, call_log)
      self.project = project
      self.call_log = call_log
    end

    def push
      check_token_validity
      create_table unless has_table? && is_table_valid?
      upload_call_data
    end

    def check_token_validity
      #TODO: IMPLEMENT ME
    end

    def upload_call_data
      columns_expr = columns.map{|name, kind| "'#{name.gsub("'", "\\'")}'"}.join(', ')

      ids = project.step_names.keys
      values = [call_log.id, call_log.address, call_log.started_at, call_log.finished_at]
      call_log.traces.each do |trace|
        values[ids.index(trace.step_id.to_i) + 4] = trace.result rescue nil
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
      project.update_attribute :current_fusion_table_id, id
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
      "#{fusion_table_name.strip.gsub(/ /,'_')}_#{index.to_s.rjust(2,'0')}"
    end

    def is_table_valid?
      current_columns = get_table_columns(current_fusion_table_id).map{|row| row[:name]}
      expected_columns = columns()
      return current_columns[0...expected_columns.length] == expected_columns
    rescue
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
      RestClient.post API_URL, {:sql => query}, {:params => {:access_token => access_token}}
    end

    def get_sql_query(query)
      RestClient.get API_URL, {:params => {:sql => query, :access_token => access_token}}
    end

    def csv_parse(csv)
      CSV.parse csv, {:headers => true, :header_converters => :symbol}
    end

    def access_token
      project.account.google_oauth_token.access_token #TODO: Refresh
    end

    def columns # TODO: Column names should be unique
      ['Call ID', 'Phone Number', ['Start Time', 'DATETIME'], ['End Time', 'DATETIME']] + project.step_names.values.map(&:to_s)
    end

  end

end
