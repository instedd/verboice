require 'spec_helper'

describe Contact do

  let!(:project) do 
    account = Account.make
    account.projects.make
  end

  it '#semicolon_separated_addresses' do
    contact = Contact.make :project => project, :addresses_attributes => [{:address => '123'}, {:address => '456'}]
    expect(contact.semicolon_separated_addresses).to eq("123;456")
  end
end