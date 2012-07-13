module ProjectHelper

  def languages_for_js
    LanguageList::COMMON_LANGUAGES.map{|l| {label: l.name, value: l.iso_639_1}}.to_json
  end

end