require "spec_helper"

describe Pushy::CLI::Client do
  describe "#reconfigure" do

    subject { given; cli_client.reconfigure }
    let(:cli_client) { Pushy::CLI::Client.new }

    context "with config file" do
      let(:given) { cli_client.config[:config_file] = config_filename }

      let(:config_file) do
        Tempfile.new('config').tap do |file|
          file.write rand(1000).to_s
          file.close
        end
      end

      let(:config_filename) { config_file.path }

      it { should be_true }
    end

    context "without config file" do
      let(:given) { nil }

      it { should be_nil }
    end
  end
end
