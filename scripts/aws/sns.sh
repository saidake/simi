#!/bin/bash


#aws sns list-topics

#aws sns create-topic --name SimiTopic
#   "TopicArn": "arn:aws:sns:us-east-1:000000000000:SimiTopic"

#aws sqs create-queue --queue-name SimiQueue
#   "TopicArn": "arn:aws:sns:us-east-1:000000000000:SimiTopic"
#   --queue-url http://localhost:4566/000000000000/SimiQueue
#aws sqs get-queue-attributes --queue-url http://localhost:4566/000000000000/SimiQueue --attribute-name QueueArn
#{
#    "Attributes": {
#        "QueueArn": "arn:aws:sqs:us-east-1:000000000000:SimiQueue"
#    }
#}

aws sqs receive-message --queue-url http://localhost:4566/000000000000/SimiQueue --max-number-of-messages 10
# Message ID: 53e48a34-748f-46e2-96c6-45bdcb9c36d1  ?

#aws sns subscribe \
#    --topic-arn "arn:aws:sns:us-east-1:000000000000:SimiTopic" \
#    --protocol sqs \
#    --notification-endpoint "arn:aws:sqs:us-east-1:000000000000:SimiQueue"
#aws sns list-subscriptions-by-topic --topic-arn "arn:aws:sns:us-east-1:000000000000:SimiTopic"

