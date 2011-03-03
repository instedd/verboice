# encoding: utf-8
Gem::Specification.new do |s|
  s.name = "batphone"
  s.version = '0.2.0'
  s.date = "2008-07-14"
  s.authors = ["Hans Fugal", "Stéphan Kochen"]
  s.email = "hans@fugal.net"
  s.summary = "Very simple and easy to learn bridge to AGI (Asterisk Gateway Interface)"
  s.homepage = "http://hans.fugal.net/src/batphone/doc/files/README.html"
  s.platform = Gem::Platform::RUBY
  s.description = "Very simple and easy to learn bridge to AGI (Asterisk Gateway Interface)"
  s.files = ["README", "README.fastagi", "ChangeLog", "lib/agi.rb", "lib/fastagi.rb"]
  s.has_rdoc = true
  s.extra_rdoc_files = ["README", "README.fastagi"]
end
