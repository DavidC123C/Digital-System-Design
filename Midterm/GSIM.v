`timescale 1ns/10ps
module GSIM ( clk, reset, in_en, b_in, out_valid, x_out);
input   clk ;
input   reset ;
input   in_en;
output  reg out_valid;
input   [15:0]  b_in;
output   [31:0]  x_out;

parameter LOOP_CNT = 80;
parameter INT_BIT = 16, FRAC_BIT = 16;

//FSM
parameter IDLE = 0, LOAD_B = 1, COMPUTE = 2, WRITE_X = 3, DONE = 4;
reg [2:0] state, nxt_state;
reg [3:0] counter_in,nxt_counter_in;
reg [6:0] counter_out,nxt_counter_out;

//Store B and x
reg [15:0] b [0:15], nxt_b[0:15];
reg [INT_BIT+FRAC_BIT-1 : 0] x [0:15], nxt_x [0:15];
wire [INT_BIT+FRAC_BIT-1 :0] x_sum;

assign x_out = x[0][INT_BIT+FRAC_BIT-1:INT_BIT+FRAC_BIT-32];

ALU alu1(.clk(clk),.reset(reset),.xi(x_sum),.b0(b[0]),.counter(counter_in),
    .x1_left(x[15]),.x1_right(x[1]),.x2_left(x[14]),.x2_right(x[2]),.x3_left(x[13]),.x3_right(x[3]));




always @(*) begin
    case (state)
        IDLE:/*if(in_en)*/nxt_state = LOAD_B;
             //else nxt_state = state;
        LOAD_B:begin
               if(counter_in == 15) nxt_state = COMPUTE;
               else nxt_state = state;
        end
        COMPUTE: begin
                    if ((&counter_in) && (counter_out == LOOP_CNT - 1)) nxt_state = WRITE_X;
                    else nxt_state = state;
                 end
        WRITE_X: begin
                    if(counter_in == 15) nxt_state = DONE;
                    else nxt_state = state;
                 end
        DONE:nxt_state = state;
        default: nxt_state = state;
    endcase
end

always @(*) begin
    nxt_counter_in = counter_in;
    nxt_counter_out = counter_out;
    case (state)
        IDLE: begin
            nxt_counter_in = 0;
            nxt_counter_out = 0;
        end
        LOAD_B:begin
            nxt_counter_in = counter_in + 1;
        end 
        COMPUTE:begin
            nxt_counter_in = counter_in + 1;
            if(&counter_in) nxt_counter_out = counter_out + 1;//當inner loop 的counter數到15 outer loop 就加一，不過可以不用counter == 15用&可以節省面積
            else begin 
                nxt_counter_out = counter_out;                     
            end
        end 
        WRITE_X:begin
            nxt_counter_in = counter_in + 1;
        end
    endcase
end
integer i;
always @(*) begin
    for (i = 0;i<16 ;i = i+1 ) begin
        nxt_b[i] = b[i];
    end
    case (state)
        LOAD_B:begin//////////要不要加x
            nxt_b[15] = b_in;
            for (i = 0 ;i < 15 ;i = i + 1 ) begin
                nxt_b[i] = b[i+1];
            end
        end
        COMPUTE:begin
            nxt_b[15] = b[0];
            for (i = 0 ;i < 15 ;i = i + 1 ) begin
                nxt_b[i] = b[i+1];
            end            
        end 
    endcase
end
always @(*) begin
    for (i = 0; i<16 ;i=i+1 ) begin
        nxt_x[i] = x[i];
    end
    case (state)
        LOAD_B: begin
            nxt_x[15] = {{2{b_in[15]}},b_in,14'b0};
            for (i =0 ;i<15 ;i=i+1 ) begin
                nxt_x[i] = x[i+1];
            end
        end
        COMPUTE:begin
            nxt_x[15] = x_sum;
            for (i =0 ;i<15 ;i=i+1) begin
                nxt_x[i] = x[i+1];
            end
        end
        WRITE_X:begin

            for (i =0 ;i<15 ;i=i+1 ) begin
                nxt_x[i] = x[i+1];
            end
        end     
    endcase
end






always @(*) begin
    if (state == WRITE_X) begin
        out_valid = 1;
    end
    else out_valid = 0;
end



always @(posedge clk or posedge reset) begin
    if (reset) begin
        state <= IDLE;
        counter_in <= 0;
        counter_out <= 0;
        for (i = 0;i<16 ;i=i+1) begin
            x[i] <= 32'b0;
            b[i] <= 16'b0;
        end
    end
    else begin
        state <= nxt_state;
        counter_in <= nxt_counter_in;
        counter_out <= nxt_counter_out;
        for (i = 0;i<16;i=i+1 ) begin
            x[i] <= nxt_x[i];
            b[i] <= nxt_b[i];
        end
    end
end





endmodule

module ALU (clk, reset, xi, counter, x1_left, x2_left, x3_left, x1_right, x2_right, x3_right, b0);
parameter INT_BIT = 16, FRAC_BIT = 16;
input clk, reset;
output signed [INT_BIT+FRAC_BIT-1 : 0] xi;
input signed [INT_BIT+FRAC_BIT-1 : 0] x1_left, x2_left, x3_left, x1_right, x2_right,x3_right;
input [3:0] counter;
input signed [15:0] b0;

parameter [3:0] case_0 = 0, case_1 = 1, case_2 = 2, case_13 = 13, case_14 = 14, case_15 = 15;
reg signed [INT_BIT+FRAC_BIT : 0] sum1, sum2, sum3;
wire signed [INT_BIT+FRAC_BIT + 2 : 0] sum6;
wire signed [INT_BIT+FRAC_BIT + 1 : 0] sumx2;
wire signed [INT_BIT+FRAC_BIT + 2 : 0] sumx1_right_6, sumx1_right_13;
wire signed [INT_BIT+FRAC_BIT + 3 : 0] sum13;
wire signed [INT_BIT+FRAC_BIT + 2 : 0] sumxb;
wire signed [INT_BIT+FRAC_BIT + 3 : 0] x_new;
wire signed [INT_BIT+FRAC_BIT + 4 : 0] sumx3;
wire signed [INT_BIT+FRAC_BIT + 4 : 0] sum_4;
wire signed [INT_BIT+FRAC_BIT + 4 : 0] sum_8;
wire signed [INT_BIT+FRAC_BIT + 4 : 0] sum_16;


always @(*) begin
    sum1 = x1_left + x1_right;
    sum2 = x2_left + x2_right;
    sum3 = x3_left + x3_right;
    case (counter)
        case_0: begin
            sum1 = x1_right;
            sum2 = x2_right;
            sum3 = x3_right; 
        end
        case_1:begin
            sum2 = x2_right;
            sum3 = x3_right;
        end
        case_2:begin
            sum3 = x3_right;
        end
        case_13:begin
            sum3 = x3_left;
        end
        case_14:begin
            sum2 = x2_left;
            sum3 = x3_left;
        end
        case_15:begin
            sum1 = x1_left;
            sum2 = x2_left;
            sum3 = x3_left;
        end
    endcase
end

assign sum6 = $signed({sum2,2'b0}) + $signed({sum2,1'b0});
assign sum13 = $signed({sum1,3'b0}) + $signed({sum1,2'b0}) + sum1;
assign sumxb = $signed({b0,{FRAC_BIT{1'b0}}});
assign x_new = b0 + sum13 - sum6 + sum3;
assign sumx3 = x_new <<< 1 + x_new;
assign sum_4 = sumx3 >>> 4 + sumx3;
assign sum_8 = sum_4 >>> 8 + sum_4;
assign sum_16 = sum_8 >>> 16 + sum_8;
assign xi = sum_16 >>> 6;

endmodule