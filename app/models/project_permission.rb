class ProjectPermission < Permission
  belongs_to :project, foreign_key: "model_id"
end
