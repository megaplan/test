cd File.dirname(__FILE__)

APPS = ['client', 'misc', 'misc_rabbit', 'nums_server']

task :default => :build_apps

task :build_apps do
  APPS.each do |app|
    cd app
    sh "rake -f ../rake/nums.rake"
    cd '../'
  end
end

task :clean do
  APPS.each do |app|
    cd app
    sh "rake -f ../rake/nums.rake clean"
    cd '../'
  end
end