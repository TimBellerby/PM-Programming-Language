

  ! isend simple
  call isend()
  
  !isend conditional
  buffer=>alloc_buffer()
  buffer%i=pack(v,mask%p==jmask)
  call isend()

  !isend array
  buffer=>alloc_buffer()
  allocate(buffer%ln(N1))
  do ib=1,N1
     buffer%ln(ib)=size(v(ib))
  enddo
  call isend(buffer%ln,jmess)
  call push_message(mess)
  do ib=1,N1
     call isend(v(iv),jmess)
     call push_message(jmess)
  enddo

  ! isend array conditional
  buffer=>alloc_buffer()
  allocate(buffer%ln(count(mask%p==jmask)))
  ibc=0
  do ib=1,N1
     if(mask%p(ib)==jmask) then
        ibc=ibc+1
        buffer%ln(ibc)=size(v(ib))
     endif
  enddo
  call isend(buffer%ln,jmess)
  call push_message(jmess)
  do ib=1,N1
     if(mask%p(ib)==jmask) then
        call isend(v(ib),jmess)
        call push_message(jmess)
     endif
  enddo
  
  ! irecv simple
  call irecv()

  ! irecv conditional
  call irecv(get_mpi_masked_type(MPI_INT,MASK,JMASK),...)
  
  ! irecv array
  call defer_recv(prc,mess_tag,N1)
  do ib=1,N1
     message_action(message_top)%p(ib)%p=>v(ib)
  enddo

  ! irecv array conditional
  call defer_recv(prc,mess_tag,count(MASK==JMASK,KIND=PM_LN))
  ibc=0
  do ib=1,N1
     if(mask%p(ib)==jmask) then
        ibc=ibc+1
        message_action(message_top)%p(ibc)%p=>v(ib)
     endif
  enddo   
