if defined? INCLUDE_DIRS
  INCLUDES = INCLUDE_DIRS.map{ |x| ' -I ' + x }.join
else
  INCLUDES = ''
end

EBIN = 'ebin'

directory EBIN

FileList['src/*.erl'].each do |src|
  dst = EBIN + '/' + File.basename(src, '.erl') + '.beam'
  file dst => [src, EBIN] do
    sh "erlc -o #{EBIN} #{INCLUDES} #{src}"
  end
  multitask :compile => dst
end

task :default => [EBIN, :compile, :copy_app]

task :copy_app do
  app_name = File.basename(Dir.pwd)
  app_file = "src/#{app_name}.app.src"
  if File.exist? app_file
    cp app_file, "#{EBIN}/#{app_name}.app"
  end
end

task :clean do
  rmtree EBIN
end
