# Create IAM policy
aws iam create-role --role-name lambda-exec-role --assume-role-policy-document file://trust-policy.json
#{
#  "Version": "2012-10-17",
#  "Statement": [
#    {
#      "Effect": "Allow",
#      "Principal": {
#        "Service": "lambda.amazonaws.com"
#      },
#      "Action": "sts:AssumeRole"
#    }
#  ]
#}
aws iam attach-role-policy --role-name lambda-exec-role --policy-arn arn:aws:iam::aws:policy/service-role/AWSLambdaBasicExecutionRole

# Create a function
aws lambda create-function \
  --function-name SimiLambdaFunction \
  --zip-file fileb://simi-lambda.zip \
  --handler org.springframework.cloud.function.adapter.aws.FunctionInvoker  \
  --runtime java17 \
  --role arn:aws:iam::123456789012:role/lambda-exec-role
# public Function<String, String> greet()
# public Function<Integer, String> square()

# Execute function with aws cli
payload='{"functionName": "greet", "data": "Alice"}'
encoded_payload=$(echo -n $payload | base64)
aws lambda invoke \
    --function-name SimiLambdaFunction \
    --payload "$encoded_payload" \
    output.json
# Update function with aws cli
#
#aws lambda update-function-code \
#  --function-name SimiLambdaFunction \
#  --zip-file fileb://simi-webflux-1.0-SNAPSHOT.jar
