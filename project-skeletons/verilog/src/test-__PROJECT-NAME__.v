
// testbench for ...
// `timescale 1ns / 1ps
module test___(skeletor-underscore-proj-name)__
  #(parameter PERIOD = 10, parameter N = 8);

   parameter HALF_P  = PERIOD/2;
   
   reg clk;
   reg [N-1:0] in;

   integer	   ip_file, op_file, code;
   
   // Outputs
   wire [N-1:0]	out;
   
   // Instantiate the Unit Under Test (UUT)
   __(skeletor-underscore-proj-name)__ #(.N(N)) mod (d, clk, n_reset, q, qbar);
   
   always #HALF_P clk = ~clk;
   
   initial begin
	  $dumpfile("wave.vcd");      // create a VCD waveform dump called "wave.vcd"
      $dumpvars(0, test___PROJECT-NAME__);
	  
      $monitor("n_reset=%d",in);

	  // Initialize Inputs
	  clk 			  = 0;
	  d 			  = 8'hx;
	  n_reset 		  = 1;

	  #HALF_P n_reset = 0; n_reset = 1;
	  #PERIOD $display("\nFinished resetting\n");

	  
      $monitor("T=%0t, clk=%d, d=%d, q=%d, qbar=%d",$time,clk,d,q,qbar);
	  ip_file = $fopen("input.txt","r");
	  op_file = $fopen("output.txt","w");
	  
	  while (!$feof(ip_file)) begin
		 code = $fscanf(ip_file, "%b\n", value);
		 d 	  = value;
		 #PERIOD;

		 if (value != d)
		   $display("%b, %d, %b, %d", d, d, value, value);
		 else
		   $display("d=%b, q=%b, qbar=%b", d, q, qbar);
		 
		 $fdisplay(op_file, "%b,%b", q, qbar);
	  end 

	  $fclose(ip_file);
	  $fclose(op_file);
      
	  $finish(); 
   end   
endmodule
