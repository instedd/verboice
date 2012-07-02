class CreateProjectVariablesFromExistingPersistedVariables < ActiveRecord::Migration
  def up
    add_column :persisted_variables, :project_variable_id, :integer
    add_index :persisted_variables, :project_variable_id

    connection.execute("SELECT DISTINCT v.name, c.project_id
      FROM persisted_variables v, contacts c
      WHERE v.contact_id = c.id").each do |name, project_id |

          sanitized_assignment = ActiveRecord::Base.send(
            :sanitize_sql_for_assignment,
            name: name,
            project_id: project_id
          )
          connection.execute("INSERT INTO project_variables SET #{sanitized_assignment}")

          connection.execute("SELECT v.id
            FROM persisted_variables v, contacts c
            WHERE v.contact_id = c.id and c.project_id = #{project_id} and v.name = '#{name}'").each do |id|

            connection.execute("UPDATE persisted_variables
              SET project_variable_id = (SELECT id
                FROM project_variables
                WHERE project_id = #{project_id}
                  AND name = '#{name}' LIMIT 1)
              WHERE id = #{id[0]}")
          end
        end

    remove_column :persisted_variables, :name
  end

  def down
    add_column :persisted_variables, :name, :string

    connection.execute("UPDATE persisted_variables v
      SET name = (SELECT name
        FROM project_variables p
        WHERE p.id = v.project_variable_id
        LIMIT 1)")

    remove_column :persisted_variables, :project_variable_id

  end
end
