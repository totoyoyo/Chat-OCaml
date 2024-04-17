
type m_type = SEND of float | ACK of float




type message = {
  t: m_type;
  data: string;
}

