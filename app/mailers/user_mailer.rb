class UserMailer < Devise::Mailer
  add_template_helper InsteddRails::MailerHelper
  layout 'mail'
end