
(* Type for messages being sent and received *)

type m_type = SEND of float | ACK of float | STOP


type message = {
  t: m_type;
  data: string;
}

