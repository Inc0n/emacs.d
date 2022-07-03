
// testbench for ...
// `timescale 1ns / 1ps
module test___(skeletor-underscore-proj-name)__
  #(parameter PERIOD = 10, parameter N = 8);

   reg clk;
   reg [N-1:0] in;
   
   // Outputs
   wire [N-1:0]	out;
   
   // Instantiate the Unit Under Test (UUT)
   // d_ff #(.N(N)) ff (d, clk, n_reset, q, qbar);
   
   always #PERIOD clk = ~clk;
   
   initial begin
	  $dumpfile("wave.vcd");      // create a VCD waveform dump called "wave.vcd"
      $dumpvars(0, test___PROJECT-NAME__);
	  
      $monitor("n_reset=%d",in);

	  // Initialize Inputs
	  clk 			  = 0;
	  d 			  = 8'hx;
	  n_reset 		  = 1;

	  #PERIOD n_reset = 0; n_reset = 1;
	  #PERIOD $display("\nFinished resetting\n");
	  
      $monitor("clk=%d, d=%d, q=%d, qbar=%d",clk,d,q,qbar);
	  #PERIOD d = 8'h0;
	  #PERIOD d = 8'h1;
	  #PERIOD d = 8'h0;
	  #PERIOD;
	  $finish(); 
   end   
endmodule
