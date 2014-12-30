# vi: set ft=ruby :
ENV['VAGRANT_DEFAULT_PROVIDER'] = 'docker'

# If you grep for env[:build_rebuild] in the vagrant code and replace it with true, you will have a better experience...
 
Vagrant.configure("2") do |config|
    config.vm.define "swsdev" do |a|
        a.vm.provider "docker" do |d|
            d.build_dir = "."
            d.build_args = ["-t=swsdev"]
            d.ports = ["3000:3000"]
            d.name = "swsdev"
            d.remains_running = true
            d.has_ssh = false
            d.cmd = ["sws", "--no-auth"]
            d.volumes = ["#{File.expand_path('.')}:/public"]
        end
    end
end
