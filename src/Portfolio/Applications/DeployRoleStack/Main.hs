module Portfolio.Applications.DeployRoleStack.Main
  ( main
  ) where

import RIO
import qualified RIO.Text as Text
import Control.Lens
import qualified System.IO as IO
import qualified Stratosphere
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import qualified Data.Aeson.QQ as Aeson.QQ
import qualified Data.Maybe as Maybe
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
import qualified Portfolio.Lib.Aeson.Main as Aeson
import qualified Portfolio.Lib.Either.Main as Either
import qualified Portfolio.Applications.DeployRoleStack.Data.App.Main as App
import qualified Portfolio.Applications.DeployRoleStack.Data.AppConfig.Main as AppConfig
import qualified Portfolio.Applications.DeployRoleStack.Data.Stack.Main as Stack

main :: IO.IO ()
main = do
  config <- Either.liftFail AppConfig.fromSystem
  App.exec config $ do
    template <- pure
      ( ( Stratosphere.template $ Stratosphere.Resources
            [ ( ( Stratosphere.iamRole
                    ( Maybe.fromMaybe mempty do
                        let iamUserArn = "arn:aws:iam::" <> config.deployRoleAwsAccountId <> ":user/" <> config.deployRoleAwsUserName
                        Aeson.toObject
                          [Aeson.QQ.aesonQQ|
                            {
                              "Version": "2012-10-17",
                              "Statement": [
                                {
                                  "Effect": "Allow",
                                  "Principal": {
                                    "AWS": #{iamUserArn}
                                  },
                                  "Action": "sts:AssumeRole"
                                }
                              ]
                            }
                          |]
                    )
                ) &
                ( Stratosphere.iamrRoleName ?~
                    ( Stratosphere.Sub
                        ( "${serviceName}-DeployRole" )
                        ( pure $ Aeson.KeyMap.fromList
                            [ ( "serviceName"
                              , Stratosphere.Literal $ Text.pack config.serviceName
                              )
                            ]
                        )
                    )
                ) &
                ( Stratosphere.iamrManagedPolicyArns ?~
                    ( Stratosphere.ValList
                        [ Stratosphere.Ref "CloudFormationDeployPolicy"
                        , Stratosphere.Ref "IamDeployPolicy"
                        , Stratosphere.Ref "LogDeployPolicy"
                        , Stratosphere.Ref "LambdaDeployPolicy"
                        , Stratosphere.Ref "ClientDeployPolicy"
                        ]
                    )
                ) &
                ( Stratosphere.resource "DeployRole" )
              )
            , ( ( Stratosphere.iamManagedPolicy
                    ( Maybe.fromMaybe mempty $ do
                        let cfnArn = "arn:aws:cloudformation:${AWS::Region}:${AWS::AccountId}:stack/" <> config.appStackName <> "*"
                        Aeson.toObject
                          [Aeson.QQ.aesonQQ|
                            {
                              "Version": "2012-10-17",
                              "Statement": [
                                {
                                  "Effect": "Allow",
                                  "Action": [
                                    "cloudformation:CreateStack",
                                    "cloudformation:UpdateStack",
                                    "cloudformation:CreateChangeSet",
                                    "cloudformation:ExecuteChangeSet",
                                    "cloudformation:DeleteChangeSet",
                                    "cloudformation:GetTemplate",
                                    "cloudformation:Describe*",
                                    "cloudformation:List*",
                                    "cloudformation:SetStackPolicy",
                                    "cloudformation:UpdateTerminationProtection"
                                  ],
                                  "Resource": {
                                    "Fn::Sub": #{cfnArn}
                                  }
                                }
                              ]
                            }
                          |]
                    )
                ) &
                ( Stratosphere.iammpManagedPolicyName ?~
                    ( Stratosphere.Sub
                        ( "${serviceName}-CloudFormationDeployPolicy" )
                        ( pure $ Aeson.KeyMap.fromList
                            [ ( "serviceName"
                              , Stratosphere.Literal $ Text.pack config.serviceName
                              )
                            ]
                        )
                    )
                ) &
                ( Stratosphere.resource "CloudFormationDeployPolicy" )
              )
            , ( ( Stratosphere.iamManagedPolicy
                    ( Maybe.fromMaybe mempty $ do
                        let lambdaArn = "arn:aws:lambda:${AWS::Region}:${AWS::AccountId}:function:" <> config.appStackName <> "-*"
                            lambdaBucketArn = "arn:aws:s3:::" <> config.lambdaBucketName
                            lambdaBucketSubPathArn = lambdaBucketArn <> "/*"
                        Aeson.toObject
                          [Aeson.QQ.aesonQQ|
                            {
                              "Version": "2012-10-17",
                              "Statement": [
                                {
                                  "Effect": "Allow",
                                  "Action": [
                                    "lambda:List*",
                                    "lambda:TagResource",
                                    "lambda:UntagResource",
                                    "lambda:AddPermission",
                                    "lambda:RemovePermission",
                                    "lambda:CreateFunction",
                                    "lambda:DeleteFunction",
                                    "lambda:GetFunction",
                                    "lambda:GetFunctionCodeSigningConfig",
                                    "lambda:GetFunctionConfiguration",
                                    "lambda:PublishVersion",
                                    "lambda:UpdateFunctionCode",
                                    "lambda:UpdateFunctionConfiguration",
                                    "lambda:PutFunctionConcurrency",
                                    "lambda:GetFunctionEventInvokeConfig",
                                    "lambda:PutFunctionEventInvokeConfig",
                                    "lambda:UpdateFunctionEventInvokeConfig",
                                    "lambda:DeleteFunctionEventInvokeConfig"
                                  ],
                                  "Resource": {
                                    "Fn::Sub": #{lambdaArn}
                                  }
                                },
                                {
                                  "Effect": "Allow",
                                  "Action": [
                                    "s3:CreateBucket",
                                    "s3:DeleteBucket",
                                    "s3:List*",
                                    "s3:GetBucketLocation",
                                    "s3:DeleteObject",
                                    "s3:GetObject",
                                    "s3:PutObject",
                                    "s3:GetObjectAcl",
                                    "s3:PutObjectAcl",
                                    "s3:PutEncryptionConfiguration",
                                    "s3:GetBucketPolicy",
                                    "s3:DeleteBucketPolicy",
                                    "s3:PutBucketPolicy",
                                    "s3:PutBucketTagging",
                                    "s3:PutObjectTagging",
                                    "s3:DeleteObjectTagging"
                                  ],
                                  "Resource": [
                                    #{lambdaBucketArn},
                                    #{lambdaBucketSubPathArn}
                                  ]
                                }
                              ]
                            }
                          |]
                    )
                ) &
                ( Stratosphere.iammpManagedPolicyName ?~
                    ( Stratosphere.Sub
                        ( "${serviceName}-LambdaDeployPolicy" )
                        ( pure $ Aeson.KeyMap.fromList
                            [ ( "serviceName"
                              , Stratosphere.Literal $ Text.pack config.serviceName
                              )
                            ]
                        )
                    )
                ) &
                ( Stratosphere.resource "LambdaDeployPolicy" )
              )
            , ( ( Stratosphere.iamManagedPolicy
                    ( Maybe.fromMaybe mempty $ do
                        let policyArn = "arn:aws:iam::${AWS::AccountId}:policy/" <> config.serviceName <> "-*"
                            roleArn = "arn:aws:iam::${AWS::AccountId}:role/" <> config.serviceName <> "-*"
                        Aeson.toObject
                          [Aeson.QQ.aesonQQ|
                            {
                              "Version": "2012-10-17",
                              "Statement": [
                                {
                                  "Effect": "Allow",
                                  "Action": [
                                    "iam:*"
                                  ],
                                  "Resource": {
                                    "Fn::Sub": #{policyArn}
                                  }
                                },
                                {
                                  "Effect": "Allow",
                                  "Action": [
                                    "iam:*"
                                  ],
                                  "Resource": {
                                    "Fn::Sub": #{roleArn}
                                  }
                                }
                              ]
                            }
                          |]
                    )
                ) &
                ( Stratosphere.iammpManagedPolicyName ?~
                    ( Stratosphere.Sub
                        ( "${serviceName}-IamDeployPolicy" )
                        ( pure $ Aeson.KeyMap.fromList
                            [ ( "serviceName"
                              , Stratosphere.Literal $ Text.pack config.serviceName
                              )
                            ]
                        )
                    )
                ) &
                ( Stratosphere.resource "IamDeployPolicy" )
              )
            , ( ( Stratosphere.iamManagedPolicy
                    ( Maybe.fromMaybe mempty $ do
                        Aeson.toObject
                          [Aeson.QQ.aesonQQ|
                            {
                              "Version": "2012-10-17",
                              "Statement": [
                                {
                                  "Effect": "Allow",
                                  "Action": [
                                    "cloudwatch:*"
                                  ],
                                  "Resource": "*"
                                },
                                {
                                  "Effect": "Allow",
                                  "Action": [
                                    "logs:*"
                                  ],
                                  "Resource": {
                                    "Fn::Sub": "arn:aws:logs:${AWS::Region}:${AWS::AccountId}:log-group:*"
                                  }
                                }
                              ]
                            }
                          |]
                    )
                ) &
                ( Stratosphere.iammpManagedPolicyName ?~
                    ( Stratosphere.Sub
                        ( "${serviceName}-LogDeployPolicy" )
                        ( pure $ Aeson.KeyMap.fromList
                            [ ( "serviceName"
                              , Stratosphere.Literal $ Text.pack config.serviceName
                              )
                            ]
                        )
                    )
                ) &
                ( Stratosphere.resource "LogDeployPolicy" )
              )
            , ( ( Stratosphere.iamManagedPolicy
                    ( Maybe.fromMaybe mempty $ do
                        let bucketArn = "arn:aws:s3:::" <> config.staticSiteBucketNamePrefix <> "-${AWS::Region}-${AWS::AccountId}"
                            bucketSubPathArn = bucketArn <> "/*"
                        Aeson.toObject
                          [Aeson.QQ.aesonQQ|
                            {
                              "Version": "2012-10-17",
                              "Statement": [
                                {
                                  "Effect": "Allow",
                                  "Action": [
                                    "cloudfront:*"
                                  ],
                                  "Resource": "*"
                                },
                                {
                                  "Effect": "Allow",
                                  "Action": [
                                    "apigateway:*"
                                  ],
                                  "Resource": "*"
                                },
                                {
                                  "Effect": "Allow",
                                  "Action": [
                                    "s3:CreateBucket",
                                    "s3:DeleteBucket",
                                    "s3:List*",
                                    "s3:GetBucketLocation",
                                    "s3:DeleteObject",
                                    "s3:GetObject",
                                    "s3:PutObject",
                                    "s3:GetObjectAcl",
                                    "s3:PutObjectAcl",
                                    "s3:PutEncryptionConfiguration",
                                    "s3:GetBucketPolicy",
                                    "s3:DeleteBucketPolicy",
                                    "s3:PutBucketPolicy",
                                    "s3:PutBucketTagging",
                                    "s3:PutObjectTagging",
                                    "s3:DeleteObjectTagging"
                                  ],
                                  "Resource": [
                                    {
                                      "Fn::Sub": #{bucketArn}
                                    },
                                    {
                                      "Fn::Sub": #{bucketSubPathArn}
                                    }
                                  ]
                                }
                              ]
                            }
                          |]
                    )
                ) &
                ( Stratosphere.iammpManagedPolicyName ?~
                    ( Stratosphere.Sub
                        ( "${serviceName}-ClientDeployPolicy" )
                        ( pure $ Aeson.KeyMap.fromList
                            [ ( "serviceName"
                              , Stratosphere.Literal $ Text.pack config.serviceName
                              )
                            ]
                        )
                    )
                ) &
                ( Stratosphere.resource "ClientDeployPolicy" )
              )
            ]
        ) &
        ( Stratosphere.templateDescription ?~ "DeployStack"
        )
      )
    Stack.fromTemplate template
    liftIO $ ByteString.Lazy.Char8.putStrLn
      ( Stratosphere.encodeTemplate template )

