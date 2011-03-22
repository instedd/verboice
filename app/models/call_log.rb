class CallLog < ActiveRecord::Base
  belongs_to :account
  belongs_to :application

  Levels = {'E' => :error, 'I' => :info, 'T' => :trace}

  def finish(state)
    self.state = state
    self.finished_at = Time.now.utc
    self.save!
  end

  def structured_details
    lines = details.split("\n")
    str = []
    last = nil
    lines.each do |line|
      if line.match /(E|I|T) (\d+(?:\.\d+)) (.*)/
        last = {:severity => Levels[$1], :time => $2, :text => $3}
        str << last
      else
        last[:text] << "\n#{line}"
      end
    end
    str
  end

  [:info, :error, :trace].each do |name|
    class_eval %Q(
      def #{name}(text)
        log '#{name.to_s[0].upcase}', text
      end
    )
  end

  private

  def log(level, text)
    details << "#{level} #{Time.now.utc - created_at} #{text}\n"
    details_will_change!
  end
end
