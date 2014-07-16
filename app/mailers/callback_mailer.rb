class CallbackMailer < ActionMailer::Base
  add_template_helper InsteddRails::MailerHelper
  layout 'mail'
  default from: Rails.configuration.verboice_configuration[:default_mailer_options][:from]

  def error(account, job, exception)
    @job = job
    @exception = exception
    @max_attempts = job.max_attempts || Delayed::Worker.max_attempts
    @attempts = job.attempts + 1
    mail(to: account.email, subject: "Callback job failed")
  end
end
