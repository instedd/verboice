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

end
