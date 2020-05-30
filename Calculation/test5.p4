header ethernet_t {
	bit<48> dstAddr;
	bit<48> srcAddr;
	bit<16> etherType;
}

header ipv4_t {
    bit<4>  version;
    bit<4>  ihl;
    bit<8>  diffserv;
    bit<16> totalLen;
    bit<16> identification;
    bit<3>  flags;
    bit<13> fragOffset;
    bit<8>  ttl;
    bit<8>  protocol;
    bit<16> hdrChecksum;
    bit<32> srcAddr;
    bit<32> dstAddr;
}

struct headers {
    ethernet_t   ethernet;
	ipv4_t       ipv4;
}

parser MyParser(packet_in packet,
                out headers hdr,
                inout metadata meta,
                inout standard_metadata_t standard_metadata) {
                
    state start {
        transition parse_ethernet;
    }

    state parse_ethernet {
        packet.extract(hdr.ethernet);
        transition select(hdr.ethernet.etherType) {
            ETHERTYPE_IPV4: parse_ipv4;
            default: accept;
        }
    }
    
    state parse_ipv4 {
        packet.extract(hdr.ipv4);
        transition accept;
    }
}

control MyIngress(inout headers hdr,
                  inout metadata meta,
                  inout standard_metadata_t standard_metadata) {
    
    action mydrop() {
        mark_to_drop();
    }
    
    action ipv4_ch() {
        hdr.ethernet.srcAddr = 2;
        hdr.ethernet.dstAddr = 1;
        hdr.ipv4.ttl = 20;
    }
    
    table ipv4_lpm {
        key = {
            hdr.ipv4.dstAddr: lpm;
        }
        actions = {
            ipv4_ch;
            mydrop;
            NoAction;
        }
        size = 1024;
        default_action = drop();
    }

    action my_drop() {
        mark_to_drop(stdmeta);
    }
    action rewrite_mac() {
        hdr.ethernet.srcAddr = 5;
        hdr.ipv4.setInvalid();
    }
    table send_frame {
        key = {
            hdr.ethernet.dstAddr: exact;
        }
        actions = {
            rewrite_mac;
            my_drop;
        }
        default_action = my_drop;
    }

    action my_drop() {
        mark_to_drop(stdmeta);
    }
    action set_bd_dmac_intf() {
        hdr.ethernet.dstAddr = 3;
        hdr.ipv4.ttl = hdr.ipv4.ttl - 1;
    }
    table mac_da {
        key = {
            meta.fwd_metadata.l2ptr: exact;
        }
        actions = {
            set_bd_dmac_intf;
            my_drop;
        }
        default_action = my_drop;
    }
    
    apply {
        if (hdr.ipv4.isValid()) {
            ipv4_lpm.apply();
        }
        else 
        {
            if(hdr.ethernet.isValid())
            {                            
                send_frame.apply();
            }
            mac_da.apply();
        }
        mac_da.apply();
    }
}

control MyDeparser(packet_out packet, in headers hdr) {
    apply {
        packet.emit(hdr.ethernet);
        packet.emit(hdr.ipv4);
    }
}