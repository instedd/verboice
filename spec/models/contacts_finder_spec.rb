require 'spec_helper'

describe ContactsFinder do

  let!(:project) { Project.make }
  let!(:contact_a) { Contact.make project: project }
  let!(:contact_b) { Contact.make project: project }
  let!(:age) { ProjectVariable.make name: 'age', project: project }
  let!(:sex) { ProjectVariable.make name: 'sex', project: project }
  let!(:diseases) { ProjectVariable.make name: 'diseases', project: project }

  let(:finder) { ContactsFinder.for(project) }

  it "should find project contacts" do
    contacts = finder.find
    contacts.size.should eq(2)
    contacts.should include(contact_a)
    contacts.should include(contact_b)
  end

  it "should not include contacts from other project" do
    other_project = Project.make
    other_contact = Contact.make project: other_project

    Contact.all.count.should eq(3)

    finder.find.should_not include(other_contact)
  end

  context "filtering" do

    it "should find contact by variable eq value" do
      PersistedVariable.make contact: contact_a, project_variable: age, value: '17'

      contacts = finder.find([
        {project_variable_id: age.id, operator: :eq, value: '17'}
      ])

      contacts.size.should eq(1)
      contacts.should include(contact_a)
    end

    it "should find contact by variable eq value" do
      PersistedVariable.make contact: contact_a, project_variable: age, value: '17'

      contacts = finder.find([
        {project_variable_id: age.id, operator: :eq, value: '17'}
      ])

      contacts.size.should eq(1)
      contacts.should include(contact_a)
    end

    it "should find contact by multiple variable eq value" do
      PersistedVariable.make contact: contact_a, project_variable: age, value: '17'
      PersistedVariable.make contact: contact_a, project_variable: sex, value: 'female'
      PersistedVariable.make contact: contact_b, project_variable: age, value: '17'

      contacts = finder.find([
        {project_variable_id: age.id, operator: :eq, value: '17'},
        {project_variable_id: sex.id, operator: :eq, value: 'female'}
      ])

      contacts.size.should eq(1)
      contacts.should include(contact_a)
    end

    it "should find contact by variable geq value" do
      PersistedVariable.make contact: contact_a, project_variable: age, value: '17'
      PersistedVariable.make contact: contact_b, project_variable: age, value: '23'

      contacts = finder.find([
        {project_variable_id: age.id, operator: :geq, value: '17'}
      ])

      contacts.size.should eq(2)
      contacts.should include(contact_a)
      contacts.should include(contact_b)
    end

    it "should find contact by variable gt value" do
      PersistedVariable.make contact: contact_a, project_variable: age, value: '17'
      PersistedVariable.make contact: contact_b, project_variable: age, value: '23'

      contacts = finder.find([
        {project_variable_id: age.id, operator: :gt, value: '17'}
      ])

      contacts.size.should eq(1)
      contacts.should include(contact_b)
    end

    it "should find contact by variable geq value using numeric ordering instead of lexicographic" do
      PersistedVariable.make contact: contact_a, project_variable: age, value: '27'
      PersistedVariable.make contact: contact_b, project_variable: age, value: '6'

      contacts = finder.find([
        {project_variable_id: age.id, operator: :geq, value: '20'}
      ])

      contacts.size.should eq(1)
      contacts.should include(contact_a)
    end

    it "should find contact by variable leq value" do
      PersistedVariable.make contact: contact_a, project_variable: age, value: '17'
      PersistedVariable.make contact: contact_b, project_variable: age, value: '23'

      contacts = finder.find([
        {project_variable_id: age.id, operator: :leq, value: '23'}
      ])

      contacts.size.should eq(2)
      contacts.should include(contact_a)
      contacts.should include(contact_b)
    end

    it "should find contact by variable lt value" do
      PersistedVariable.make contact: contact_a, project_variable: age, value: '17'
      PersistedVariable.make contact: contact_b, project_variable: age, value: '23'

      contacts = finder.find([
        {project_variable_id: age.id, operator: :lt, value: '23'}
      ])

      contacts.size.should eq(1)
      contacts.should include(contact_a)
    end

    it "should find variable by defined" do
      PersistedVariable.make contact: contact_a, project_variable: age, value: '17'

      contacts = finder.find([
        {project_variable_id: age.id, operator: :defined}
      ])

      contacts.size.should eq(1)
      contacts.should include(contact_a)
    end

    it "should find variable by undefined when variable is missing" do
      PersistedVariable.make contact: contact_a, project_variable: age, value: '17'

      contacts = finder.find([
        {project_variable_id: age.id, operator: :undefined}
      ])

      contacts.size.should eq(1)
      contacts.should include(contact_b)
    end

    it "should find variable by undefined when variable is null" do
      PersistedVariable.make contact: contact_a, project_variable: age, value: '17'
      PersistedVariable.make contact: contact_b, project_variable: age, value: nil

      contacts = finder.find([
        {project_variable_id: age.id, operator: :undefined}
      ])

      contacts.size.should eq(1)
      contacts.should include(contact_b)
    end

    it "should find contacts with undefined or blank variable when searching for blank value" do
      PersistedVariable.make contact: contact_a, project_variable: age, value: ''

      contacts = finder.find([
        {project_variable_id: age.id, operator: :eq, value: ''}
      ])

      contacts.size.should eq(2)
      contacts.should include(contact_a)
      contacts.should include(contact_b)
    end

    it "should find variable by includes value" do
      PersistedVariable.make contact: contact_a, project_variable: diseases, value: 'malaria h1n1 cholera'
      PersistedVariable.make contact: contact_b, project_variable: diseases, value: 'malaria cholera'

      contacts = finder.find([
        {project_variable_id: diseases.id, operator: :includes, value: 'h1n1'}
      ])

      contacts.size.should eq(1)
      contacts.should include(contact_a)

      contacts = finder.find([
        {project_variable_id: diseases.id, operator: :includes, value: 'cholera'}
      ])

      contacts.size.should eq(2)
      contacts.should include(contact_a)
      contacts.should include(contact_b)
    end

    it "should find variable by includes numerical value" do
      PersistedVariable.make contact: contact_a, project_variable: diseases, value: 'malaria 1010'
      PersistedVariable.make contact: contact_b, project_variable: diseases, value: 'malaria 2010'

      contacts = finder.find([
        {project_variable_id: diseases.id, operator: :includes, value: '20'}
      ])

      contacts.size.should eq(1)
      contacts.should include(contact_b)

      contacts = finder.find([
        {project_variable_id: diseases.id, operator: :includes, value: '10'}
      ])

      contacts.size.should eq(2)
      contacts.should include(contact_a)
      contacts.should include(contact_b)
    end

    it "should find with implicit variables" do
      PersistedVariable.make contact: contact_a, implicit_key: 'language', value: 'en'
      PersistedVariable.make contact: contact_b, implicit_key: 'language', value: 'es'

      contacts = finder.find([
        {implicit_key: 'language', operator: :eq, value: 'es'}
      ])

      contacts.size.should eq(1)
      contacts.should include(contact_b)
    end

    it "should find with variable compared to variable" do
      other = ProjectVariable.make name: 'other', project: project

      PersistedVariable.make contact: contact_a, project_variable: age, value: '17'
      PersistedVariable.make contact: contact_a, project_variable: other, value: '20'

      PersistedVariable.make contact: contact_b, project_variable: age, value: '23'
      PersistedVariable.make contact: contact_b, project_variable: other, value: '20'

      contacts = finder.find([
        {project_variable_id: age.id, operator: :geq, other_project_variable_id: other.id}
      ])

      contacts.size.should eq(1)
      contacts.should include(contact_b)
    end

    it "should find with variable compared to implicit variable" do
      PersistedVariable.make contact: contact_a, project_variable: age, value: '17'
      PersistedVariable.make contact: contact_a, implicit_key: 'other', value: '20'

      PersistedVariable.make contact: contact_b, project_variable: age, value: '23'
      PersistedVariable.make contact: contact_b, implicit_key: 'other', value: '20'

      contacts = finder.find([
        {project_variable_id: age.id, operator: :leq, other_implicit_key: 'other'}
      ])

      contacts.size.should eq(1)
      contacts.should include(contact_a)
    end

    it "should find with variable equal to variable" do
      other = ProjectVariable.make name: 'other', project: project

      PersistedVariable.make contact: contact_a, project_variable: age, value: '17'
      PersistedVariable.make contact: contact_a, project_variable: other, value: '20'

      PersistedVariable.make contact: contact_b, project_variable: age, value: '23'
      PersistedVariable.make contact: contact_b, project_variable: other, value: '23'

      contacts = finder.find([
        {project_variable_id: age.id, operator: :eq, other_project_variable_id: other.id}
      ])

      contacts.size.should eq(1)
      contacts.should include(contact_b)
    end

    context "by address" do

      let!(:contact_c) { Contact.make project: project }
      let!(:contact_d) { Contact.make project: project }

      before(:each) do
        ContactAddress.delete_all

        contact_a.addresses.create!(project_id: project.id, address: '30')
        contact_a.addresses.create!(project_id: project.id, address: '10')

        contact_b.addresses.create!(project_id: project.id, address: '15')
        contact_b.addresses.create!(project_id: project.id, address: '40')

        contact_c.addresses.create!(project_id: project.id, address: '20')
        contact_c.addresses.create!(project_id: project.id, address: '60')
      end

      it "should find contacts by address equal to a value" do
        contacts = finder.find([
          {field_name: "address", operator: :eq, value: '15'}
        ])

        contacts.should include(contact_b)
      end

      it "should find contacts by address containing a numerical value" do
        contacts = finder.find([
          {field_name: "address", operator: :includes, value: '3'}
        ])

        contacts.should include(contact_a)
      end

      it "should find contacts by address defined" do
        contacts = finder.find([
          {field_name: "address", operator: :defined}
        ])

        contacts.should include(contact_a, contact_b, contact_c)
      end

      it "should find contacts by address undefined" do
        contacts = finder.find([
          {field_name: "address", operator: :undefined}
        ])

        contacts.should include(contact_d)
      end

      it "should find contacts by address greater than a value" do
        contacts = finder.find([
          {field_name: "address", operator: :geq, value: '35'}
        ])

        contacts.should include(contact_b, contact_c)
      end

      it "should find contacts by address equal to a variable" do
        PersistedVariable.make contact: contact_a, project_variable: age, value: '30'
        PersistedVariable.make contact: contact_b, project_variable: age, value: '35'

        contacts = finder.find([
          {field_name: "address", operator: :eq, other_project_variable_id: age.id}
        ])

        contacts.should include(contact_a)
      end

    end

  end

  context "sorting" do

    let!(:contact_c) { Contact.make project: project }
    let!(:contact_d) { Contact.make project: project }

    it "should sort contacts by persisted variable" do
      PersistedVariable.make contact: contact_a, project_variable: age, value: '30'
      PersistedVariable.make contact: contact_b, project_variable: age, value: '10'
      PersistedVariable.make contact: contact_c, project_variable: age, value: '20'
      PersistedVariable.make contact: contact_d, project_variable: age, value: '15'

      contacts = finder.find([], {sorting: {project_variable_id: age.id, direction: 'ASC'}})
      contacts.map(&:id).should eq([contact_b, contact_d, contact_c, contact_a].map(&:id))
    end

    it "should sort contacts by persisted variable including nulls" do
      PersistedVariable.make contact: contact_a, project_variable: age, value: '30'
      PersistedVariable.make contact: contact_b, project_variable: age, value: '10'
      PersistedVariable.make contact: contact_c, project_variable: age, value: '20'

      contacts = finder.find([], {sorting: {project_variable_id: age.id, direction: 'ASC'}})
      contacts.map(&:id).should eq([contact_d, contact_b, contact_c, contact_a].map(&:id))
    end

    it "should sort contacts by persisted variable in descending order" do
      PersistedVariable.make contact: contact_a, project_variable: age, value: '30'
      PersistedVariable.make contact: contact_b, project_variable: age, value: '10'
      PersistedVariable.make contact: contact_c, project_variable: age, value: '20'
      PersistedVariable.make contact: contact_d, project_variable: age, value: '15'

      contacts = finder.find([], {sorting: {project_variable_id: age.id, direction: 'DESC'}})
      contacts.map(&:id).should eq([contact_a, contact_c, contact_d, contact_b].map(&:id))
    end

    it "should sort contacts by address" do
      ContactAddress.delete_all
      contact_a.addresses.create(project_id: project.id, address: '30')
      contact_b.addresses.create(project_id: project.id, address: '10')
      contact_c.addresses.create(project_id: project.id, address: '20')
      contact_d.addresses.create(project_id: project.id, address: '15')

      contacts = finder.find([], {sorting: {address: true, direction: 'ASC'}})
      contacts.map(&:id).should eq([contact_b, contact_d, contact_c, contact_a].map(&:id))
    end

    it "should sort contacts by address in descending order" do
      ContactAddress.delete_all
      contact_a.addresses.create(project_id: project.id, address: '30')
      contact_b.addresses.create(project_id: project.id, address: '10')
      contact_c.addresses.create(project_id: project.id, address: '20')
      contact_d.addresses.create(project_id: project.id, address: '15')

      contacts = finder.find([], {sorting: {address: true, direction: 'DESC'}})
      contacts.map(&:id).should eq([contact_a, contact_c, contact_d, contact_b].map(&:id))
    end

    it "should sort contacts by address handling multiple and no addresses using the first one" do
      ContactAddress.delete_all

      contact_a.addresses.create(project_id: project.id, address: '30')

      contact_b.addresses.create(project_id: project.id, address: '10')
      contact_b.addresses.create(project_id: project.id, address: '40')

      contact_c.addresses.create(project_id: project.id, address: '20')
      contact_c.addresses.create(project_id: project.id, address: '60')

      contacts = finder.find([], {sorting: {address: true, direction: 'ASC'}})
      contacts.map(&:id).should eq([contact_d, contact_b, contact_c, contact_a].map(&:id))
    end

    it "should sort contacts by implicit variable" do
      # Ensure implicit vars are properly loaded
      ImplicitVariables::SmsNumber; ImplicitVariables::Language

      PersistedVariable.make contact: contact_a, implicit_key: 'language', value: 'it'
      PersistedVariable.make contact: contact_b, implicit_key: 'language', value: 'en'
      PersistedVariable.make contact: contact_c, implicit_key: 'language', value: 'es'
      PersistedVariable.make contact: contact_d, implicit_key: 'sms_number', value: '15'

      contacts = finder.find([], {sorting: {implicit_key: 'language', direction: 'ASC'}})
      contacts.map(&:id).should eq([contact_d, contact_b, contact_c, contact_a].map(&:id))
    end

    it "should sort contacts by last activity" do
      contact_a.update_column :last_activity_at, 5.days.ago
      contact_b.update_column :last_activity_at, 8.days.ago
      contact_c.update_column :last_activity_at, nil
      contact_d.update_column :last_activity_at, 1.day.ago

      contacts = finder.find([], {sorting: {last_activity: true, direction: 'ASC'}})
      contacts.map(&:id).should eq([contact_c, contact_b, contact_a, contact_d].map(&:id))
    end

  end

end
