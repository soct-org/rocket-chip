module EICG_wrapper(
  output logic out,
  input  logic en,
  input  logic test_en,
  input  logic in
);

  logic en_latched /*verilator clock_enable*/;

  always_latch begin
    if (!in) begin
      en_latched = en || test_en;
    end
    // implicit retain (latch) when 'in' is high
  end

  assign out = en_latched && in;

endmodule
