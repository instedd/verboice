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
      @str << "|" if @str.present?
      @str << "#{action}\\s*=\>\\s*#{value}\\s*"
    end

    def remove(section)
      @removes << section.to_s
    end

    def flush
      target = Tempfile.new 'asterisk_conf', "#{Rails.root}/tmp"
      section = nil

      File.open @file, 'r' do |file|
        while(line = file.gets)
          case line
          when /^\s*\[(.*)\]/
            if @add_actions.has_key? section
              @add_actions[section].each do |actions|
                actions.each { |x| target.puts x }
              end
              target.puts
            end
            section = $1
            if options = @adds[section]
              write_section section, options, target
              @adds.delete section
            end
          end

          if @removes.exclude? section
            if @remove_actions.has_key? section
              if line =~ /^\s*#{@remove_actions[section]}/
                next
              end
            end

            target.write line
          end

        end
      end

      @adds.each do |section, options|
        write_section section, options, target
      end
    ensure
      target.close
      FileUtils.mv target, @file
    end

    def write_section(section, options, target)
      target.puts "[#{section}]"
      options.each do |key, values|
        values = *values
        values.each do |value|
          target.puts "#{key}=#{value}"
        end
      end
      target.puts
    end
  end
end
