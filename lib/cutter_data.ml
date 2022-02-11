type header = { dur: int; scale: int }
type track = { dur: int }
type movie = {
  mvhd : header;
  traks : track list
}

