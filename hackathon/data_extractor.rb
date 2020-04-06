#!/usr/bin/ruby

require 'open-uri'
require 'nokogiri'
require 'net/https'
require 'csv'

OpenSSL::SSL::VERIFY_PEER = OpenSSL::SSL::VERIFY_NONE

INSTITUTS = ['isma.ivanovo.ru',
             'ispu.ru',
             'ivgsha.ru',
             'ivgpu.com',
             'ivanovo.ac.ru',
             'isuct.ru']

employees_uri = '/sveden/employees'
EDUCATION_URI = '/sveden/education'

WEB_ARCHIVE_URL = 'web.archive.org'
WEB_ARCHIVE_API_URI = '/cdx/search/cdx?url='
WEB_ARCHIVE_WEB_URI = '/web'

def fetch_timestamps(url, uri)
  timestemps = []
  http = Net::HTTP.new(WEB_ARCHIVE_URL)
  response = http.get(WEB_ARCHIVE_API_URI + url + uri)
  response.body.each_line{|line| timestemps.push(line.split(' ')[1])}
  return timestemps
end

def filter_timestamps(timestemps)
  timestamps_hash = {}
  timestemps.each{|timestamp| timestamps_hash[timestamp.slice(0, 4)] = timestamp}
  return timestamps_hash
end

def url_gen(timestamp, url, uri)
  "http://#{WEB_ARCHIVE_URL}#{WEB_ARCHIVE_WEB_URI}/#{timestamp}/#{url}#{uri}"
end

def fetch_employees_data(url)
  puts url
  html = Nokogiri::HTML(open(url))
  n = 0
  data = []
  html.css('tr').each do |tr|
    if tr.at_css('[@itemprop = "fio"]') && (tr.at_css('[@itemprop = "Degree"]') || tr.at_css('[@itemprop = "degree"]'))
      n += 1
      fio = tr.at_css('[@itemprop = "fio"]').children.text.strip.gsub(/\s{2,}/, " ")
      degree = tr.at_css('[@itemprop = "Degree"]').children.text.strip if tr.at_css('[@itemprop = "Degree"]')
      degree = tr.at_css('[@itemprop = "degree"]').children.text.strip if tr.at_css('[@itemprop = "degree"]')
      gen_experience = tr.at_css('[@itemprop = "genExperience"]').children.text.strip if tr.at_css('[@itemprop = "genExperience"]')
      gen_experience = tr.at_css('[@itemprop = "GenExperience"]').children.text.strip if tr.at_css('[@itemprop = "GenExperience"]')
      spec_experience = tr.at_css('[@itemprop = "specExperience"]').children.text.strip if tr.at_css('[@itemprop = "specExperience"]')
      spec_experience = tr.at_css('[@itemprop = "specExperience"]').children.text.strip if tr.at_css('[@itemprop = "specExperience"]')
      degree = case degree 
      when /доктор|Доктор|д\.|Д\./
        'доктор наук'
      when /канд|Канд|к\.|К\./
        'кандидат наук'
      else
        'нет степени'
      end
      gen_experience = gen_experience.to_i == 0 ? 'NaN' : gen_experience.to_i
      spec_experience = spec_experience.to_i == 0 ? 'NaN' : spec_experience.to_i
      data << {fio: fio, degree: degree, gen_experience: gen_experience, spec_experience: spec_experience}
    end
  end
  return data
end

csv = CSV.open("../data/hackathon/data.csv", 'wb')
csv << (['fio', 'degree', 'gen_experience', 'spec_experience', 'year', 'url', 'status'])
data_array = []
start_year = ''
last_year = ''
INSTITUTS.each do |url|
  puts url
  employees_uri = url == 'ivgpu.com' ? '/sveden/employees/faculty' : '/sveden/employees'
  timestemps = fetch_timestamps(url, employees_uri)
  filter_timestamps(timestemps).each do |year, timestamp|
    last_year = year
    puts year
    n = 0
    fetch_employees_data(url_gen(timestamp, url, employees_uri)).each do |line|
      n += 1
      unless line.values.compact.empty?
        data_array << line.merge({year: year, url: url})
      end
    end
    puts "Добавлено #{n} строк"
  end
  if last_year.to_i != Time.now.year
    year = Time.now.year
    puts year
    n = 0
    fetch_employees_data('http://' + url + employees_uri).each do |line|
      n += 1
      unless line.values.compact.empty?
        data_array << line.merge({year: year, url: url})
      end
    end
    puts "Добавлено #{n} строк"
  end
end
data_array = data_array.sort_by{|line| [line[:url], line[:fio], line[:year].to_i]}
data_array.each_with_index do |line, index|
  start_year = line[:year] if index == 0 || data_array[index][:url] != data_array[index - 1][:url]
  if index == 0 || line[:year] == start_year || line[:year] == last_year
    line[:status] = 'работает'
  else
    if line[:fio] == data_array[index - 1][:fio]
      line[:status] = 'работает'
    else
      if line[:fio] != data_array[index + 1][:fio]
        line[:status] = 'уволен'
      else
        line[:status] = 'принят на работу'
      end
    end
  end
end
data_array.each{|line| csv << line.values}