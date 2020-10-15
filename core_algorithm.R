calculate_rho <- function(){
  return(flow.rate / chamber.size)
}

calculate_first_term <- function(c.0, rho, alpha, t.enclosure){
  return(c.0 * exp(-(rho * alpha) * t.enclosure))
}

calculate_second_term <- function(){
  return(c.in + ((production.rate + alpha * (c.ideal - c.in)) / (production.rate + alpha)))
}
  
calculate_third_term <- function(rho, alpha, t.enclosure){
  return(1 - exp(-(rho * alpha) * t.enclosure))
}

calculate_ct <- function(c.0, rho, alpha, production.rate, c.ideal, c.in, t.enclosure){
  first_term <- calculate_first_term()
  second_term <- calculate_second_term()
  third_term <- calculate_third_term()

  return(first_term + second_term * third_term)
}
  
  
  
