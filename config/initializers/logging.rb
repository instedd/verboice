require 'formatted_rails_logger'

#monkey-patch BufferedLogger
FormattedRailsLogger.patch_rails

#Use the supplied formatter that includes timestamp and severity
Rails.logger.formatter = FormattedRailsLogger::Formatter.new