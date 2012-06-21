Delayed::Worker.delay_jobs = Rails.env.production?
Delayed::Worker.destroy_failed_jobs = true
Delayed::Worker.sleep_delay = 60
Delayed::Worker.max_attempts = 5
Delayed::Worker.max_run_time = 5.minutes
