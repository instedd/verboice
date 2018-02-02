require "spec_helper"

describe ContactsImporter do
  let(:account) { Account.make }
  let(:project) { Project.make }

  describe "save_csv" do

    def uploaded_file(contents)
      file = Tempfile.open(['contacts', '.csv'])
      file.write(contents)
      file.close

      uploaded = Rack::Test::UploadedFile.new(file.path, 'text/csv')
      allow(uploaded).to receive(:tempfile).and_return(uploaded)
      uploaded
    end

    before(:each) do
      @importer = ContactsImporter.new account, project
      FileUtils.mkdir_p ContactsImporter::TmpDir
    end

    it "should succeed with a valid CSV file" do
      valid_file = uploaded_file "Phone,foo,bar\n1,2,3\n"
      expect(@importer.save_csv(valid_file)).to be_nil
      expect(File.file?(@importer.csv_filename)).to be(true)
    end

    it "should return false for nil" do
      expect(@importer.save_csv(nil)).to eq("You didn't specify a CSV file")
    end

    it "should return false for an empty file" do
      empty_file = uploaded_file ''
      expect(@importer.save_csv(empty_file)).to eq("The CSV file must not be empty")
    end

    it "should return false for an invalid CSV file" do
      invalid_file = uploaded_file "foo,bar\n\""
      expect(@importer.save_csv(invalid_file)).to eq("Error reading CSV: Unclosed quoted field on line 2.")
    end

    it "should return false if not all rows have the same number of columns" do
      invalid_file = uploaded_file "foo,bar\na,b\nc"
      expect(@importer.save_csv(invalid_file)).to eq("All rows in the CSV must have the same number of columns")
    end

  end

  describe "guess_column_specs" do

    context "when there are no contact vars" do
      before(:each) do
        @importer = ContactsImporter.new account, project
      end

      it "returns phone and new_variable" do
        @importer.rows_preview = [['Phone', 'Color'], ['123', 'Foobar']]

        specs = @importer.guess_column_specs
        expect(specs.size).to eq(2)
        expect(specs[0][:action]).to eq('phone_number')
        expect(specs[0][:name]).to eq('Phone number')
        expect(specs[1][:action]).to eq('new_variable')
        expect(specs[1][:name]).to eq('Color')
      end

      it "ignores columns with blank headers" do
        @importer.rows_preview = [['Phone', 'Color', ''], ['123', 'Foobar', nil]]

        specs = @importer.guess_column_specs
        expect(specs.last[:action]).to eq('ignore')
      end
    end

    context "where there are contact vars and implicit vars" do
      before(:each) do
        @var_color = project.project_variables.make name: "color"
        @var_age = project.project_variables.make name: "age"

        @importer = ContactsImporter.new account, project
      end

      it "returns existing field specs for columns with same name as existing" do
        @importer.rows_preview = [['Phone', 'Color', 'Age', 'Language'], ['123', 'Foobar', 'Lala', 'Spanish']]

        specs = @importer.guess_column_specs
        expect(specs.size).to eq(4)
        expect(specs[0][:action]).to eq('phone_number')
        expect(specs[0][:name]).to eq('Phone number')
        expect(specs[1][:action]).to eq('existing_variable')
        expect(specs[1][:name]).to eq('color')
        expect(specs[1][:id]).to eq(@var_color.id)
        expect(specs[2][:action]).to eq('existing_variable')
        expect(specs[2][:name]).to eq('age')
        expect(specs[2][:id]).to eq(@var_age.id)
        expect(specs[3][:action]).to eq('existing_variable')
        expect(specs[3][:name]).to eq('language')
      end
    end
  end


  context "import" do

    def create_saved_file(importer, contents)
      path = importer.csv_filename
      CSV.open(path, 'wb') do |csv|
        contents.each do |row|
          csv << row
        end
      end
    end


    it "creates new vars and new contacts" do
      importer = ContactsImporter.new account, project
      importer.column_specs = [{'action' => 'phone_number'}, {'action' => 'new_variable', 'name' => 'age'}]
      create_saved_file importer, [["Phone", "Age"], ["+(12) 3-4", "56"], ["hello", "78"]]

      importer.import

      vars = project.project_variables.all
      expect(vars.length).to eq(1)

      var = vars[0]
      expect(var.name).to eq("age")

      contacts = project.contacts.all
      expect(contacts.length).to eq(2)

      contact = contacts[0]
      addresses = contact.addresses.all
      expect(addresses.length).to eq(1)

      address = addresses[0]
      expect(address.address).to eq("1234")

      persisted_vars = contact.persisted_variables.all
      expect(persisted_vars.length).to eq(1)

      persisted_var = persisted_vars[0]
      expect(persisted_var.project_variable_id).to eq(var.id)
      expect(persisted_var.value).to eq("56")

      contact = contacts[1]
      addresses = contact.addresses.all
      expect(addresses.length).to eq(1)

      address = addresses[0]
      expect(address.address).to eq("hello")

      persisted_vars = contact.persisted_variables.all
      expect(persisted_vars.length).to eq(1)

      persisted_var = persisted_vars[0]
      expect(persisted_var.project_variable_id).to eq(var.id)
      expect(persisted_var.value).to eq("78")
    end

    it "creates new contacts with large file" do
      importer = ContactsImporter.new account, project
      importer.column_specs = [{'action' => 'phone_number'}, {'action' => 'new_variable', 'name' => 'age'}]

      data = 250.times.map { |x| [(999000 + x).to_s, x] }
      create_saved_file importer, [["Phone", "Age"]] + data

      importer.import

      vars = project.project_variables.all
      expect(vars.length).to eq(1)

      var = vars[0]
      expect(var.name).to eq("age")

      contacts = project.contacts.all
      expect(contacts.length).to eq(250)

      contacts.each do |contact|
        expect(contact).to have(1).addresses
        expect(contact.addresses.first.address[0..2]).to eq('999')
        expect(contact).to have(1).persisted_variables
      end
    end

    %w(new_variable existing_variable).each do |action|
      it "imports into existing variables with action '#{action}'" do
        var_age = project.project_variables.make name: "age"

        importer = ContactsImporter.new account, project
        importer.column_specs = [{'action' => 'phone_number'}, {'action' => action, 'name' => 'age'}]
        create_saved_file importer, [["Phone", "Age"], ["+(12) 3-4", "56"], ["hello", "78"]]
        importer.import

        vars = project.project_variables.all
        expect(vars.length).to eq(1)

        var = vars[0]
        expect(var.name).to eq("age")

        contacts = project.contacts.all
        expect(contacts.length).to eq(2)

        contact = contacts[0]
        addresses = contact.addresses.all
        expect(addresses.length).to eq(1)

        address = addresses[0]
        expect(address.address).to eq("1234")

        persisted_vars = contact.persisted_variables.all
        expect(persisted_vars.length).to eq(1)

        persisted_var = persisted_vars[0]
        expect(persisted_var.project_variable_id).to eq(var.id)
        expect(persisted_var.value).to eq("56")

        contact = contacts[1]
        addresses = contact.addresses.all
        expect(addresses.length).to eq(1)

        address = addresses[0]
        expect(address.address).to eq("hello")

        persisted_vars = contact.persisted_variables.all
        expect(persisted_vars.length).to eq(1)

        persisted_var = persisted_vars[0]
        expect(persisted_var.project_variable_id).to eq(var.id)
        expect(persisted_var.value).to eq("78")
      end
    end

    it "imports into existing contact" do
      var_age = project.project_variables.make name: "age"

      contact = project.contacts.new
      contact.addresses.new address: "1234"
      contact.save!

      importer = ContactsImporter.new account, project
      importer.column_specs = [{'action' => 'phone_number'}, {'action' => 'new_variable', 'name' => 'age'}]
      create_saved_file importer, [["Phone", "Age"], ["+(12) 3-4", "56"]]
      importer.import

      vars = project.project_variables.all
      expect(vars.length).to eq(1)

      var = vars[0]
      expect(var.name).to eq("age")

      contacts = project.contacts.all
      expect(contacts.length).to eq(1)

      contact = contacts[0]
      addresses = contact.addresses.all
      expect(addresses.length).to eq(1)

      address = addresses[0]
      expect(address.address).to eq("1234")

      persisted_vars = contact.persisted_variables.all
      expect(persisted_vars.length).to eq(1)

      persisted_var = persisted_vars[0]
      expect(persisted_var.project_variable_id).to eq(var.id)
      expect(persisted_var.value).to eq("56")
    end

    it "imports into existing implicit variable" do
      importer = ContactsImporter.new account, project
      importer.column_specs = [{'action' => 'phone_number'}, {'action' => 'existing_variable', 'name' => 'language'}]
      create_saved_file importer, [["Phone", "Language"], ["+(12) 3-4", "es"]]
      importer.import

      vars = project.project_variables.all
      expect(vars.length).to eq(0)

      contacts = project.contacts.all
      expect(contacts.length).to eq(1)

      contact = contacts[0]
      addresses = contact.addresses.all
      expect(addresses.length).to eq(1)

      address = addresses[0]
      expect(address.address).to eq("1234")

      persisted_vars = contact.persisted_variables.all
      expect(persisted_vars.length).to eq(1)

      persisted_var = persisted_vars[0]
      expect(persisted_var.project_variable_id).to be_nil
      expect(persisted_var.implicit_key).to eq("language")
      expect(persisted_var.value).to eq("es")
    end

    it "imports into existing contact with existing variable value" do
      var_age = project.project_variables.make name: "age"

      contact = project.contacts.new
      contact.addresses.new address: "1234"
      contact.persisted_variables.new project_variable_id: var_age.id, value: "23"
      contact.save!

      importer = ContactsImporter.new account, project
      importer.column_specs = [{'action' => 'phone_number'}, {'action' => 'new_variable', 'name' => 'age'}]
      create_saved_file importer, [["Phone", "Age"], ["+(12) 3-4", "56"]]
      importer.import

      vars = project.project_variables.all
      expect(vars.length).to eq(1)

      var = vars[0]
      expect(var.name).to eq("age")

      contacts = project.contacts.all
      expect(contacts.length).to eq(1)

      contact = contacts[0]
      addresses = contact.addresses.all
      expect(addresses.length).to eq(1)

      address = addresses[0]
      expect(address.address).to eq("1234")

      persisted_vars = contact.persisted_variables.all
      expect(persisted_vars.length).to eq(1)

      persisted_var = persisted_vars[0]
      expect(persisted_var.project_variable_id).to eq(var.id)
      expect(persisted_var.value).to eq("56")
    end
  end
end
