require 'test_helper'

class AsteriskConfTest < ActiveSupport::TestCase
  TmpFileName = "#{Rails.root}/tmp/sip.conf"

  setup do
    FileUtils.cp "#{Rails.root}/test/fixtures/sip.conf", TmpFileName
  end

  teardown do
    FileUtils.rm TmpFileName
  end

  test "remove section" do
    Asterisk::Conf.change TmpFileName do
      remove :gateway1
    end

    assert_result 'remove'
  end

  test "add section when not already present" do
    Asterisk::Conf.change TmpFileName do
      add :gateway2, :type => :peer, :context => :verboice, :allow => [:foo, :bar]
    end

    assert_result 'add_when_not_already_present'
  end

  test "add section when already present" do
    Asterisk::Conf.change TmpFileName do
      add :gateway1, :type => :peer, :context => :verboice, :allow => [:foo, :bar]
    end

    assert_result 'add_when_already_present'
  end

  test "add section when already present and last" do
    Asterisk::Conf.change TmpFileName do
      add :gateway3, :type => :peer, :context => :verboice, :allow => [:foo, :bar]
    end

    assert_result 'add_when_already_present_and_last'
  end

  test "add register to global section" do
    Asterisk::Conf.change TmpFileName do
      add_action :general, :register, 'xxx:yyy@zzz.com'
    end

    assert_result 'add_action'
  end

  test "remove register from global section" do
    Asterisk::Conf.change TmpFileName do
      remove_action :general, :register, 'foo:bar@host.com'
    end

    assert_result 'remove_action'
  end

  def assert_result(file)
    result = `diff #{Rails.root}/test/fixtures/#{file}.conf #{TmpFileName}`
    assert_equal 0, $?.exitstatus, result
  end
end
