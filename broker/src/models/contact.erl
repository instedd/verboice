-module(contact).
-export ([find_or_create_with_address/2, create_anonymous/2]).
-define(TABLE_NAME, "contacts").
-include_lib("erl_dbmodel/include/model.hrl").

find_or_create_with_address(ProjectId, Address) ->
  case contact_address:find([{project_id, ProjectId}, {address, Address}]) of
    undefined ->
      Contact = contact:create(#contact{project_id = ProjectId}),
      contact_address:create(#contact_address{project_id = ProjectId, contact_id = Contact#contact.id, address = Address}),
      Contact;
    #contact_address{contact_id = ContactId} ->
      contact:find(ContactId)
  end.

create_anonymous(ProjectId, Address) ->
  Contact = contact:create(#contact{project_id = ProjectId, anonymous = 1}),
  contact_address:create(#contact_address{project_id = ProjectId, contact_id = Contact#contact.id, address = Address}),
  Contact.
