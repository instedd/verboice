class CallLog < ActiveRecord::Base
  include CallLogSearch

  belongs_to :account
  belongs_to :project
  belongs_to :channel
  belongs_to :schedule
  has_many :traces, :foreign_key => "call_id"

  before_validation :set_account_to_project_account, :if => :project_id?

  validates_presence_of :account
  validates_presence_of :project
  validates_presence_of :channel

  Levels = {'E' => :error, 'W' => :warn, 'I' => :info, 'T' => :trace}

  def state
    read_attribute(:state).try(:to_sym)
  end

  def direction
    read_attribute(:direction).try(:to_sym)
  end

  def outgoing?
    direction == :outgoing
  end

  def start_incoming
    start
  end

  def start_outgoing(address)
    self.address = address
    info "Calling #{address}"
    start
  end

  def start
    self.state = :active
    self.started_at = Time.now.utc
    self.save!
  end

  def finish_with_error(message)
    error message
    finish :failed
  end

  def finish_successfully
    project.push_results(self) unless project.nil?
    finish :completed
  end

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
      if line.match /(E|W|I|T) (\d+(?:\.\d+)) (.*)/
        last = {:severity => Levels[$1], :time => Time.at($2.to_f).utc, :text => $3}
        str << last
      else
        last[:text] << "\n#{line}"
      end
    end
    str
  end

  Levels.each do |letter, name|
    class_eval %Q(
      def #{name}(text)
        log '#{letter}', text
      end
    )
  end

  private

  def log(level, text)
    self.details ||= ""
    self.details += "#{level} #{Time.now.utc.to_f} #{text}\n"
  end

  def set_account_to_project_account
    self.account_id = self.project.account_id
  end
end
