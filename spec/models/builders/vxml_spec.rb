# Copyright (C) 2010-2012, InSTEDD
#
# This file is part of Verboice.
#
# Verboice is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Verboice is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Verboice.  If not, see <http://www.gnu.org/licenses/>.

require 'spec_helper'

describe Builders::Vxml do

  let(:callback) { 'http://www.domain.com/foo' }
  let(:vxml) { Builders::Vxml.new callback }
  let(:vars) { Hash.new }

  it "builds an empty vxml" do
    vxml.build.should be_equivalent_to(build_xml)
  end

  it "builds a prompt for says" do
    inner_xml = <<-XML
      <prompt>Hello World</prompt>
    XML

    vxml.say("Hello World").build.should be_equivalent_to(build_xml(inner_xml))
  end

  it "builds a prompt with break for pause" do
    inner_xml = <<-XML
      <prompt>
        <break time="5s"/>
      </prompt>
    XML

    vxml.pause(5).build.should be_equivalent_to(build_xml(inner_xml))
  end

  it "should hangup with exit within catch event noinput/nomatch" do
    inner_xml = <<-XML
      <catch event="noinput nomatch">
        <exit/>
      </catch>
    XML

    vxml.hangup.build.should be_equivalent_to(build_xml(inner_xml))
  end

  it "should play using audio" do
    inner_xml = <<-XML
      <audio src="foo.wav"/>
    XML

    vxml.play("foo.wav").build.should be_equivalent_to(build_xml(inner_xml))
  end

  it "builds a callback using submit" do
    inner_xml = <<-XML
      <catch event="noinput nomatch">
        <submit next="http://www.foo.com/bar" method="GET" namelist="digits"/>
      </catch>
    XML

    vxml.callback("http://www.foo.com/bar").build.should be_equivalent_to(build_xml(inner_xml))
  end

  it "builds a POST callback using submit" do
    inner_xml = <<-XML
      <catch event="noinput nomatch">
        <submit next="http://www.foo.com/bar" method="POST" namelist="digits"/>
      </catch>
    XML

    vxml.callback("http://www.foo.com/bar", :post).build.should be_equivalent_to(build_xml(inner_xml))
  end

  it 'builds var for default variables' do
    vars.merge!({:key1 => 'value1', :key2 => 'value2'})
    outside_form = <<-XML
      <var name="key1" expr="'value1'"/>
      <var name="key2" expr="'value2'"/>
    XML

    Builders::Vxml.new(callback, vars).build.should be_equivalent_to(build_xml(nil,nil,outside_form))
  end

  context "capture" do

    before(:all) do
      @inner_xml = <<-XML
        <catch event="noinput nomatch">
          <goto next="#capture_form"/>
        </catch>
      XML
    end

    it "should capture with say using field" do
      capture_form_inner_xml = <<-XML
        <field name="digits" type="digits?minlength=1;maxlength=1">
          <property name="timeout" value="10s"/>
          <noinput><assign name="digits" expr="''"/></noinput>
          <nomatch><assign name="digits" expr="''"/></nomatch>
          <prompt>This is a capture</prompt>
        </field>
      XML

      vxml.capture({:say => "This is a capture"}).build.should be_equivalent_to(build_xml(@inner_xml, capture_form_inner_xml))
    end

    it "should capture with play using field" do
      capture_form_inner_xml = <<-XML
        <field name="digits" type="digits?minlength=1;maxlength=1">
          <property name="timeout" value="10s"/>
          <noinput><assign name="digits" expr="''"/></noinput>
          <nomatch><assign name="digits" expr="''"/></nomatch>
          <audio src="capture.wav"/>
        </field>
      XML

      vxml.capture({:play => "capture.wav"}).build.should be_equivalent_to(build_xml(@inner_xml, capture_form_inner_xml))
    end

    it "should capture with options using field" do
      capture_form_inner_xml = <<-XML
        <field name="digits" type="digits?minlength=5;maxlength=10">
          <property name="timeout" value="20s"/>
          <noinput><assign name="digits" expr="''"/></noinput>
          <nomatch><assign name="digits" expr="''"/></nomatch>
          <prompt>This is a capture</prompt
        </field>
      XML

      vxml.capture(:say => "This is a capture", :min => 5, :max => 10, :timeout => 20).build.should be_equivalent_to(build_xml(@inner_xml, capture_form_inner_xml))
    end

    context 'after capture' do
      before :each do
        @capture_form_inner_xml = <<-XML
          <field name="digits" type="digits?minlength=1;maxlength=1">
            <property name="timeout" value="10s"/>
            <noinput><assign name="digits" expr="''"/></noinput>
            <nomatch><assign name="digits" expr="''"/></nomatch>
            <prompt>This is a capture</prompt>
          </field>
        XML
      end

      it 'should callback with submit in block' do
        @capture_form_inner_xml << <<-XML
          <block>
            <submit next="http://www.foo.com/bar" method="GET" namelist="digits"/>
          </block>
        XML

        expected_xml = build_xml @inner_xml, @capture_form_inner_xml

        vxml.capture({:say => "This is a capture"}).callback("http://www.foo.com/bar").build.should be_equivalent_to(expected_xml)
      end

    end
  end

  def build_xml(inner_xml = '', capture_form_inner_xml = '', outside_form = '')
    error_vars_list = vars.keys.concat(%w(error message event)).join(' ')
    disconnect_vars_list = vars.keys.concat(%w(disconnect)).join(' ')
    <<-XML
      <?xml version="1.0" encoding="UTF-8"?>
      <vxml version="2.1">
        <catch event="connection.disconnect.hangup">
          <var name="disconnect" expr="true"/>
          <submit next="#{callback}" namelist="#{disconnect_vars_list}"/>
          <exit/>
        </catch>
        <error>
          <var name="error" expr="true"/>
          <var name="message" expr="_message"/>
          <var name="event" expr="_event"/>
          <submit next="#{callback}" namelist="#{error_vars_list}"/>
          <exit/>
        </error>
        #{outside_form}
        <form>
          <field name="dummy">
            <property name="timeout" value="0s"/>
            <grammar type="text/gsl">[agrammarthatwillnevermatch]</grammar>

            #{inner_xml}
          </field>
        </form>
        <form id="capture_form">
          #{capture_form_inner_xml}
        </form>
      </vxml>
    XML
  end

end