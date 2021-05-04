export default ({
    cardCVC,
    cardExpirationMonth,
    cardExpirationYear,
    cardNumber,
    ...rest
  }) => ({
  ...rest,
  cardNumberLastFourDigits: cardNumber.substr(cardNumber.length - 4)
})
