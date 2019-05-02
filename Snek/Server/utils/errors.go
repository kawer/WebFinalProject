package utils

type RequestError struct {
	Code    int
	Message string
}

func (err *RequestError) Error() string {
	return err.Message
}
