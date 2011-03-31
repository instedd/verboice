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
    end

    private

    def process_file(target)
      section = nil

      File.open @file, 'r' do |file|
        while line = file.gets
          if line =~ /^\s*\[(.*)\]/
            process_add_actions section, target
            section = $1
            process_adds section, target
          end

          target.write line unless removed? section, line
        end

        process_add_actions section, target

        write_adds target
      end
    end

    def process_add_actions(section, target)
      if @add_actions.has_key? section
        @add_actions[section].each do |actions|
          actions.each { |x| target.puts x }
        end
        target.puts
      end
    end

    def process_adds(section, target)
      if options = @adds[section]
        write_section section, options, target
        @adds.delete section
      end
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

    def write_adds(target)
      @adds.each { |section, options| write_section section, options, target }
    end

    def write_section(section, options, target)
      target.puts "[#{section}]"
      options.each do |key, values|
        Array(values).each { |value| target.puts "#{key}=#{value}" }
      end
      target.puts
    end
  end
end
