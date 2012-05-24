require "spec_helper"

describe Pushy::Client do
  include SpecHelpers::Config

  describe '.from_json' do
    let(:pushy_client) { Pushy::Client.from_json(config_json) }

    def self.its(_attribute, &expectation)
      context "with configuration attribute :#{_attribute}" do
        subject { pushy_client.send(_attribute) }
        it('should set attribute', &expectation)
      end
    end

    its(:node_name)   { should eql host }
    its(:in_address)  { should eql in_addr }
    its(:out_address) { should eql out_addr }
    its(:interval)    { should eql interval }

    its(:server_public_key)  { should eql public_key }
  end
end
