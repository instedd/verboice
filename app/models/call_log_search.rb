module CallLogSearch
  extend ActiveSupport::Concern

  module ClassMethods
    def search(search, options = {})
      result = where '1 = 1'

      search = Search.new search

      if search.search
        result = result.where "call_logs.id = :search OR call_logs.address = :search OR call_logs.state = :search OR call_logs.direction = :search", :search => search.search
      end

      if search[:id]
        op, val = Search.get_op_and_val search[:id]
        result = result.where "call_logs.id #{op} ?", val.to_i
      end

      result = result.where "direction = ?", search[:direction] if search[:direction]
      result = result.where "state = ?", search[:state] if search[:state]

      [:address, :caller, :caller_id].each do |sym|
        result = result.where "address = ?", search[sym] if search[sym]
      end

      if search[:after]
        after = Time.smart_parse search[:after]
        result = result.where "started_at >= ?", after if after
      end
      if search[:before]
        before = Time.smart_parse search[:before]
        result = result.where "started_at <= ?", before if before
      end

      if search[:channel]
        if options[:account]
          channel = options[:account].channels.select(:id).find_by_name search[:channel]
          if channel
            result = result.where :channel_id => channel.id
          else
            result = result.where '1 = 2'
          end
        else
          result = result.joins(:channel).where 'channels.name = ?', search[:channel]
        end
      end
      if search[:application]
        if options[:account]
          app = options[:account].applications.select(:id).find_by_name search[:application]
          if app
            result = result.where :application_id => app.id
          else
            result = result.where '1 = 2'
          end
        else
          result = result.joins(:application).where 'applications.name = ?', search[:application]
        end
      end

      result
    end
  end
end
