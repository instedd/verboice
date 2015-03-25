require "spec_helper"
require "fakefs/spec_helpers"

describe ContactsImporter do
  let(:account) { Account.make }
  let(:project) { Project.make }

  describe "save_csv" do
    include FakeFS::SpecHelpers

    before(:each) do
      @importer = ContactsImporter.new account, project
      FileUtils.mkdir_p ContactsImporter::TmpDir
    end

    it "should succeed with a valid CSV file" do
      valid_file = double()
      valid_file.stub(:read) { "Phone,foo,bar\n1,2,3\n" }
      @importer.save_csv(valid_file).should be_nil
      File.file?(@importer.csv_filename).should be_true
    end

    it "should return false for nil" do
      @importer.save_csv(nil).should eq("You didn't specify a CSV file")
    end

    it "should return false for an empty file" do
      empty_file = double()
      empty_file.stub(:read) { '' }
      @importer.save_csv(empty_file).should eq("CSV file must not be empty")
    end

    it "should return false for an invalid CSV file" do
      invalid_file = double()
      invalid_file.stub(:read) { "foo,bar\n\"" }
      @importer.save_csv(invalid_file).should eq("Invalid CSV: Unclosed quoted field on line 2.")
    end

    it "should return false if not all rows have the same number of columns" do
      invalid_file = double()
      invalid_file.stub(:read) { "foo,bar\na,b\nc" }
      @importer.save_csv(invalid_file).should eq("Not all rows have the same number of columns")
    end

    it "should return false if no Phone column" do
      invalid_file = double()
      invalid_file.stub(:read) { "foo,bar\na, b" }
      @importer.save_csv(invalid_file).should eq("Missing 'Phone number' column")
    end
  end

  describe "guess_column_specs" do
    context "when there are no contact vars" do
      before(:each) do
        @importer = ContactsImporter.new account, project
      end

      it "returns phone and new_variable" do
        @importer.rows = [['Phone', 'Color'], ['123', 'Foobar']]

        specs = @importer.guess_column_specs
        specs.size.should == 2
        specs[0][:action].should == 'phone_number'
        specs[0][:name].should == 'Phone number'
        specs[1][:action].should == 'new_variable'
        specs[1][:name].should == 'Color'
      end

      it "ignores columns with blank headers" do
        @importer.rows = [['Phone', 'Color', ''], ['123', 'Foobar', nil]]

        specs = @importer.guess_column_specs
        specs.last[:action].should == 'ignore'
      end
    end

    context "where there are contact vars and implicit vars" do
      before(:each) do
        @var_color = project.project_variables.make name: "color"
        @var_age = project.project_variables.make name: "age"

        @importer = ContactsImporter.new account, project
      end

      it "returns existing field specs for columns with same name as existing" do
        @importer.rows = [['Phone', 'Color', 'Age', 'Language'], ['123', 'Foobar', 'Lala', 'Spanish']]

        specs = @importer.guess_column_specs
        specs.size.should == 4
        specs[0][:action].should == 'phone_number'
        specs[0][:name].should == 'Phone number'
        specs[1][:action].should == 'existing_variable'
        specs[1][:name].should == 'color'
        specs[1][:id].should eq(@var_color.id)
        specs[2][:action].should == 'existing_variable'
        specs[2][:name].should == 'age'
        specs[2][:id].should eq(@var_age.id)
        specs[3][:action].should == 'existing_variable'
        specs[3][:name].should == 'language'
      end
    end

    context "import" do
      it "creates new vars and new contacts" do
        importer = ContactsImporter.new account, project
        importer.column_specs = [{'action' => 'phone_number'}, {'action' => 'new_variable', 'name' => 'age'}]
        importer.rows = [["Phone", "Age"], ["+(12) 3-4", "56"], ["hello", "78"]]
        importer.import

        vars = project.project_variables.all
        vars.length.should eq(1)

        var = vars[0]
        var.name.should eq("age")

        contacts = project.contacts.all
        contacts.length.should eq(2)

        contact = contacts[0]
        addresses = contact.addresses.all
        addresses.length.should eq(1)

        address = addresses[0]
        address.address.should eq("1234")

        persisted_vars = contact.persisted_variables.all
        persisted_vars.length.should eq(1)

        persisted_var = persisted_vars[0]
        persisted_var.project_variable_id.should eq(var.id)
        persisted_var.value.should eq("56")

        contact = contacts[1]
        addresses = contact.addresses.all
        addresses.length.should eq(1)

        address = addresses[0]
        address.address.should eq("hello")

        persisted_vars = contact.persisted_variables.all
        persisted_vars.length.should eq(1)

        persisted_var = persisted_vars[0]
        persisted_var.project_variable_id.should eq(var.id)
        persisted_var.value.should eq("78")
      end

      %w(new_variable existing_variable).each do |action|
        it "imports into existing variables with action '#{action}'" do
          var_age = project.project_variables.make name: "age"

          importer = ContactsImporter.new account, project
          importer.column_specs = [{'action' => 'phone_number'}, {'action' => action, 'name' => 'age'}]
          importer.rows = [["Phone", "Age"], ["+(12) 3-4", "56"], ["hello", "78"]]
          importer.import

          vars = project.project_variables.all
          vars.length.should eq(1)

          var = vars[0]
          var.name.should eq("age")

          contacts = project.contacts.all
          contacts.length.should eq(2)

          contact = contacts[0]
          addresses = contact.addresses.all
          addresses.length.should eq(1)

          address = addresses[0]
          address.address.should eq("1234")

          persisted_vars = contact.persisted_variables.all
          persisted_vars.length.should eq(1)

          persisted_var = persisted_vars[0]
          persisted_var.project_variable_id.should eq(var.id)
          persisted_var.value.should eq("56")

          contact = contacts[1]
          addresses = contact.addresses.all
          addresses.length.should eq(1)

          address = addresses[0]
          address.address.should eq("hello")

          persisted_vars = contact.persisted_variables.all
          persisted_vars.length.should eq(1)

          persisted_var = persisted_vars[0]
          persisted_var.project_variable_id.should eq(var.id)
          persisted_var.value.should eq("78")
        end
      end

      it "imports into existing contact" do
        var_age = project.project_variables.make name: "age"

        contact = project.contacts.new
        contact.addresses.new address: "1234"
        contact.save!

        importer = ContactsImporter.new account, project
        importer.column_specs = [{'action' => 'phone_number'}, {'action' => 'new_variable', 'name' => 'age'}]
        importer.rows = [["Phone", "Age"], ["+(12) 3-4", "56"]]
        importer.import

        vars = project.project_variables.all
        vars.length.should eq(1)

        var = vars[0]
        var.name.should eq("age")

        contacts = project.contacts.all
        contacts.length.should eq(1)

        contact = contacts[0]
        addresses = contact.addresses.all
        addresses.length.should eq(1)

        address = addresses[0]
        address.address.should eq("1234")

        persisted_vars = contact.persisted_variables.all
        persisted_vars.length.should eq(1)

        persisted_var = persisted_vars[0]
        persisted_var.project_variable_id.should eq(var.id)
        persisted_var.value.should eq("56")
      end

      it "imports into existing implicit variable" do
        importer = ContactsImporter.new account, project
        importer.column_specs = [{'action' => 'phone_number'}, {'action' => 'existing_variable', 'name' => 'language'}]
        importer.rows = [["Phone", "Language"], ["+(12) 3-4", "es"]]
        importer.import

        vars = project.project_variables.all
        vars.length.should eq(0)

        contacts = project.contacts.all
        contacts.length.should eq(1)

        contact = contacts[0]
        addresses = contact.addresses.all
        addresses.length.should eq(1)

        address = addresses[0]
        address.address.should eq("1234")

        persisted_vars = contact.persisted_variables.all
        persisted_vars.length.should eq(1)

        persisted_var = persisted_vars[0]
        persisted_var.project_variable_id.should be_nil
        persisted_var.implicit_key.should eq("language")
        persisted_var.value.should eq("es")
      end

      it "imports into existing contact with existing variable value" do
        var_age = project.project_variables.make name: "age"

        contact = project.contacts.new
        contact.addresses.new address: "1234"
        contact.persisted_variables.new project_variable_id: var_age.id, value: "23"
        contact.save!

        importer = ContactsImporter.new account, project
        importer.column_specs = [{'action' => 'phone_number'}, {'action' => 'new_variable', 'name' => 'age'}]
        importer.rows = [["Phone", "Age"], ["+(12) 3-4", "56"]]
        importer.import

        vars = project.project_variables.all
        vars.length.should eq(1)

        var = vars[0]
        var.name.should eq("age")

        contacts = project.contacts.all
        contacts.length.should eq(1)

        contact = contacts[0]
        addresses = contact.addresses.all
        addresses.length.should eq(1)

        address = addresses[0]
        address.address.should eq("1234")

        persisted_vars = contact.persisted_variables.all
        persisted_vars.length.should eq(1)

        persisted_var = persisted_vars[0]
        persisted_var.project_variable_id.should eq(var.id)
        persisted_var.value.should eq("56")
      end
    end
  end
end
