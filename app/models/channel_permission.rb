class ChannelPermission < Permission
  belongs_to :channel, foreign_key: "model_id"
end
