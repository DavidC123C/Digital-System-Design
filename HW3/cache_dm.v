module cache(
    clk,
    proc_reset,
    proc_read,
    proc_write,
    proc_addr,
    proc_rdata,
    proc_wdata,
    proc_stall,
    mem_read,
    mem_write,
    mem_addr,
    mem_rdata,
    mem_wdata,
    mem_ready
);
    
//==== input/output definition ============================
    input          clk;
    // processor interface
    input          proc_reset;
    input          proc_read, proc_write;
    input   [29:0] proc_addr;
    input   [31:0] proc_wdata;
    output         proc_stall;
    output reg [31:0] proc_rdata;
    // memory interface
    input  [127:0] mem_rdata;
    input          mem_ready;
    output         mem_read, mem_write;
    output  [27:0] mem_addr;
    output [127:0] mem_wdata;
    
//==== wire/reg definition ================================
parameter IDLE = 2'b00;
parameter Compare_Tag = 2'b01;
parameter Write_Back = 2'b10;
parameter Allocate = 2'b11;
parameter word = 6'd32;

integer i;

wire [1:0] word_offset;
wire [2:0] proc_index_in;
wire [24:0] proc_addr_tag;
wire       hit;
reg  [7:0] dirty, nxt_dirty;
reg  [7:0] valid, nxt_valid;
reg  [1:0] state, nxt_state;
reg  [24:0] Tag [0:7];
reg  [24:0] nxt_Tag [0:7];
reg  [127:0] block [0:7] ;
reg  [127:0] nxt_block [0:7] ;

//==== combinational circuit ==============================
assign word_offset = proc_addr[1:0];
assign proc_index_in = proc_addr[4:2];
assign proc_addr_tag = proc_addr[29:5];
assign mem_addr = (state == Write_Back) ? {Tag[proc_index_in],proc_index_in}:
                  (state == Allocate)? proc_addr[29:2]: 28'b0;

///////hit
assign hit = ((proc_addr_tag == Tag[proc_index_in]) && valid[proc_index_in]) ? 1'b1 : 1'b0; 

always @(*) begin
    if(proc_write && hit)begin
        case (word_offset)
            2'd0: nxt_block[proc_index_in] [word-1:0] = proc_wdata;
            2'd1: nxt_block[proc_index_in] [2*word-1:word] = proc_wdata;
            2'd2: nxt_block[proc_index_in] [3*word-1:2*word] = proc_wdata;
            2'd3: nxt_block[proc_index_in] [4*word-1:3*word] = proc_wdata;
            default: nxt_block[proc_index_in] [word-1:0] = proc_wdata;
        endcase
    end
    else  nxt_block[proc_index_in] [word-1:0] = block[proc_index_in] [word-1:0];
end
/////////////proc_read  ///////
always @(*) begin
    if (proc_read && hit) begin 
        case (word_offset)
            2'd0: proc_rdata = block[proc_index_in][word-1:0];
            2'd1: proc_rdata = block[proc_index_in][2*word-1:word];
            2'd2: proc_rdata = block[proc_index_in][3*word-1:2*word];
            2'd3: proc_rdata = block[proc_index_in][4*word-1:3*word];
            default: proc_rdata = block[proc_index_in][word-1:0];
        endcase
    end
    else proc_rdata = 32'b0;
end
////////mem_wdata
assign mem_wdata = (state == Write_Back) ? block[proc_index_in] : 128'b0;

///////mem_rdata
always @(*) begin
    for (i = 0;i<8 ;i=i+1) begin
    nxt_block[i] = block[i];
    end
    if(state == Allocate) nxt_block[proc_index_in] = mem_rdata;
    else nxt_block[proc_index_in] = block[proc_index_in];
end

always @(*) begin
    nxt_dirty = dirty;
    if (proc_write && hit)  nxt_dirty[proc_index_in] = 1'b1;
    else if (state == Write_Back && mem_ready) nxt_dirty[proc_index_in] = 1'b0;
    else nxt_dirty[proc_index_in] = dirty[proc_index_in];
end

/////mem_read
assign mem_read = (state == Allocate ) ? 1 : 0; 
/////mem_write
assign mem_write = (state == Write_Back) ? 1 : 0;
////Tag
always @(*) begin
    for (i = 0;i<8 ;i=i+1) begin
        nxt_Tag[i] = Tag[i];
    end
    case (state)
        Allocate:begin
            if(mem_ready) nxt_Tag[proc_index_in] = proc_addr_tag;
            else nxt_Tag[proc_index_in] = Tag[proc_index_in];
        end
        default: nxt_Tag[proc_index_in] = Tag[proc_index_in];
    endcase
end

////////////valid
always @(*) begin
    nxt_valid = valid;
    if((state == Allocate && (mem_ready))) nxt_valid[proc_index_in] = 1'b1;
    else nxt_valid[proc_index_in] = valid[proc_index_in];
end
/////////stall
assign proc_stall = (state == IDLE && hit) ? 1'b0 : 1'b1;
//////FSM//////
always @(*) begin
    case (state)
        IDLE:begin
            if (proc_read || proc_write) nxt_state = Compare_Tag;
            else nxt_state = state;
        end
        Compare_Tag:begin
            if(hit) nxt_state = IDLE;
            else if(!hit && !(dirty[proc_index_in])) nxt_state = Allocate;
            else if(!hit && dirty[proc_index_in])nxt_state = Write_Back;
            else nxt_state = state;
        end   
        Write_Back:begin
            if(mem_ready) nxt_state = Allocate;
            else nxt_state = state;
        end
        Allocate:begin
            if(mem_ready) nxt_state = Compare_Tag;
            else nxt_state = state;
        end
        default: nxt_state = state;
    endcase
end


//==== sequential circuit =================================
always@( posedge clk ) begin
    if( proc_reset ) begin
    state <= 0;
    valid <= 0;
    dirty <= 0;
    block[0] <= 0;
    block[1] <= 0;
    block[2] <= 0;
    block[3] <= 0;
    block[4] <= 0;
    block[5] <= 0;
    block[6] <= 0;
    block[7] <= 0;
    Tag[0] <= 0;
    Tag[1] <= 0;
    Tag[2] <= 0;
    Tag[3] <= 0;
    Tag[4] <= 0;
    Tag[5] <= 0;
    Tag[6] <= 0;
    Tag[7] <= 0;    
    end
    else begin
    state <= nxt_state;
    valid <= nxt_valid;
    dirty <= nxt_dirty;
    block[0] <= nxt_block[0];
    block[1] <= nxt_block[1];
    block[2] <= nxt_block[2];
    block[3] <= nxt_block[3];
    block[4] <= nxt_block[4];
    block[5] <= nxt_block[5];
    block[6] <= nxt_block[6];
    block[7] <= nxt_block[7];
    Tag[0]   <= nxt_Tag[0];
    Tag[1]   <= nxt_Tag[1];
    Tag[2]   <= nxt_Tag[2];
    Tag[3]   <= nxt_Tag[3];
    Tag[4]   <= nxt_Tag[4];
    Tag[5]   <= nxt_Tag[5];
    Tag[6]   <= nxt_Tag[6];
    Tag[7]   <= nxt_Tag[7];
    end
end

endmodule
