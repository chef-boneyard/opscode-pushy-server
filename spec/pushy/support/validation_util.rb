require 'date'

shared_context "validation_util" do
    let(:valid_datetime) { ->(v) { Time.parse(v) } }

    def validate_datetime(field)

       expect{
          Time.parse(field)
       }.to_not raise_error

    end

    def validate_time_ordering(field1, field2)
      expect {
        time1 = Time.parse(field1)
        time2 = Time.parse(field2)
        (time2 - time2).should >= 0
      }.to_not raise_error
    end
end
