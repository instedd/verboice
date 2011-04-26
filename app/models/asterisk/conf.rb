module Asterisk
  class Conf
    def self.change(file, &block)
      conf = Conf.new file
      conf.instance_eval &block
      conf.flush
    end

    def initialize(file)
      @file = file
      @adds = {}
      @removes = []
      @add_actions = Hash.new { |h1, k1| h1[k1] = [] }
      @remove_actions = Hash.new { |h1, k1| h1[k1] = "" }
    end

    def add(section, options = {})
      remove section
      @adds[section.to_s] = options
    end

    def add_action(section, action, value)
      @add_actions[section.to_s] << "#{action} => #{value}"
    end

    def remove_action(section, action, value)
      @str = @remove_actions[section.to_s]
      @str << "|" if @str.length > 0
      @str << "#{action}\\s*=\>\\s*#{value}\\s*"
    end
    alias_method :delete_action, :remove_action

    def remove(section)
      @removes << section.to_s
    end
    alias_method :delete, :remove

    def flush
      target = Tempfile.new 'asterisk_conf'
      process_file target
    ensure
      target.close
      FileUtils.mv target, @file
      File.chmod 0644, @file
    end

    private

    def process_file(target)
      section = nil

      File.foreach @file do |line|
        if line =~ /^\s*\[(.*)\]/ # That is, a section header is found
          write_actions section, target if has_action? section
          section = $1
          write_add section, target if has_add? section
        end

        check_remove_add_action section, line
        target.write line unless removed? section, line
      end

      write_actions section, target if has_action? section
      write_remaining_added_sections target
    end

    def has_action?(section, line = nil)
      @add_actions[section].present?
    end

    def has_add?(section)
      @adds.has_key? section
    end

    def check_remove_add_action(section, line)
      return unless @add_actions.has_key? section

      @add_actions[section].each do |action|
        if line =~ /^\s*#{action}\s*/
          @add_actions[section].delete action
          return
        end
      end
    end

    def write_actions(section, target)
      @add_actions[section].each do |actions|
        actions.each { |x| target.puts x }
      end
      target.puts
    end

    def write_add(section, target)
      write_section section, @adds[section], target
      @adds.delete section
    end

    def process_removes(section, line, target)
      if @removes.exclude? section
        unless @remove_actions.has_key?(section) && line =~ /^\s*#{@remove_actions[section]}/
          target.write line
        end
      end
    end

    def removed?(section, line)
      @removes.include?(section) || (@remove_actions.has_key?(section) && line =~ /^\s*#{@remove_actions[section]}/)
    end

    def write_remaining_added_sections(target)
      @adds.each { |section, options| write_section section, options, target }
    end

    def write_section(section, options, target)
      target.write "[#{section}]"

      template = options.delete :template
      target.write "(#{template})" if template

      target.puts
      options.each do |key, values|
        Array(values).each { |value| target.puts "#{key}=#{value}" }
      end
      target.puts
    end
  end
end
