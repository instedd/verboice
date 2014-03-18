class Permission < ActiveRecord::Base
  attr_accessible :account_id, :role, :model_id, :type
  belongs_to :account

  def self.for_project_ids_and_channel_ids(project_ids, channel_ids)
    where("(type = ? AND model_id IN (?)) OR (type = ? AND model_id IN (?))",
        "Project", project_ids, "Channel", channel_ids)
  end

  # STI: we store "Channel" and "Project" in the type column,
  # we get ChannelPermission and ProjectPermission

  def self.find_sti_class(type_name)
    "#{type_name}Permission".constantize
  end

  def self.sti_name
    super[0 ... -10]
  end
end
