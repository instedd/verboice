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
end
